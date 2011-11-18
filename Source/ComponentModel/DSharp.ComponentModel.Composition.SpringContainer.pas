(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.ComponentModel.Composition.SpringContainer;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.Core.Lazy,
  Generics.Collections,
  Rtti,
  Spring.Container,
  Spring.Container.Core,
  Spring.Container.Injection,
  Spring.Container.Registration,
  SysUtils,
  TypInfo;

type
  TSpringContainer = class(TContainer, IServiceLocator, IInterface)
  private
    FExportedProperties: TDictionary<string, TRttiProperty>;
    FInterfaces: TDictionary<TGUID, TRttiInterfaceType>;
    function CreateFieldInjectionDelegate(const propertyName: string): TFunc<TValue>;
    function CreatePropertyInjectionDelegate(const propertyName: string): TFunc<TValue>;
    procedure ImportMember(Model: TComponentModel; Member: TRttiMember);
    function RegisterClass(ClassType: TRttiType): TRegistration;
    procedure RegisterClassImplementingInterface(ClassType: TRttiInstanceType);
    procedure RegisterClassInheritedExport(ClassType: TRttiInstanceType);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ImportRtti;

    function Resolve(TypeInfo: PTypeInfo; const Name: string): TValue; overload;

    function ResolveLazy<T>: ILazy<T>; overload;
    function ResolveLazy(LazyType: TRttiType): TValue; overload;
    function ResolveAllLazy<TServiceType>: TArray<ILazy<TServiceType>>; overload;
    function ResolveAllLazy(LazyType: TRttiType): TArray<TValue>; overload;
  end;

type
  TPropertyInjectionWithDelegate = class(TPropertyInjection)
  private
    fDelegate: TFunc<TValue>;
  protected
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  public
    constructor Create(model: TComponentModel; const targetName: string;
      delegate: TFunc<TValue>);
  end;

  TFieldInjectionWithDelegate = class(TFieldInjection)
  private
    fDelegate: TFunc<TValue>;
  protected
    procedure DoInject(instance: TObject; const arguments: array of TValue); override;
  public
    constructor Create(model: TComponentModel; const targetName: string;
      delegate: TFunc<TValue>);
  end;

implementation

uses
  DSharp.Core.Reflection,
  Spring.Collections;

{$IF COMPILERVERSION = 21}
type
  TGuidHelper = record helper for TGUID
    class function Empty: TGUID; static;
  end;

class function TGuidHelper.Empty: TGUID;
begin
  FillChar(Result, Sizeof(Result), 0)
end;
{$IFEND}

{ TSpringContainer }

constructor TSpringContainer.Create;
begin
  inherited;
  FExportedProperties := TDictionary<string, TRttiProperty>.Create();
end;

function TSpringContainer.CreateFieldInjectionDelegate(
  const propertyName: string): TFunc<TValue>;
begin
  Result :=
    function: TValue
    var
      LProperty: TRttiProperty;
    begin
      if FExportedProperties.TryGetValue(propertyName, LProperty) then
        Result := LProperty.GetValue(Resolve(LProperty.Parent.Handle).AsObject)
      else
        Result := TValue.Empty;
    end;
end;

function TSpringContainer.CreatePropertyInjectionDelegate(const propertyName: string): TFunc<TValue>;
begin
  Result :=
    function: TValue
    var
      LProperty: TRttiProperty;
    begin
      if FExportedProperties.TryGetValue(propertyName, LProperty) then
        Result := LProperty.GetValue(Resolve(LProperty.Parent.Handle).AsObject)
      else
        Result := TValue.Empty;
    end;
end;

destructor TSpringContainer.Destroy;
begin
  FExportedProperties.Free();
  inherited;
end;

procedure TSpringContainer.ImportMember(Model: TComponentModel; Member: TRttiMember);
var
  LImportAttribute: ImportAttribute;
  LInjection: IInjection;
  LValue: TValue;
begin
  if Member.TryGetAttributeOfType<ImportAttribute>(LImportAttribute) then
  begin
    if Member.RttiType.IsGenericTypeOf('Lazy') then
    begin
      LValue := ResolveLazy(Member.RttiType);
      if Member is TRttiProperty then
        Model.InjectProperty(Member.Name, LValue)
      else if Member is TRttiField then
        Model.InjectField(Member.Name, LValue);
    end else
    if Member.RttiType.IsGenericTypeOf('TArray')
      and Member.RttiType.GetGenericArguments[0].IsGenericTypeOf('Lazy') then
    begin
      LValue := TValue.FromArray(Member.RttiType.Handle,
        ResolveAllLazy(Member.RttiType.GetGenericArguments[0]));
      if Member is TRttiProperty then
        Model.InjectProperty(Member.Name, LValue)
      else if Member is TRttiField then
        Model.InjectField(Member.Name, LValue);
    end else
    begin
      if LImportAttribute.Name <> '' then
      begin
//        LModel := ComponentRegistry.GetComponent(LType.Handle); ???

        if Member is TRttiProperty then
        begin
          LInjection := TPropertyInjectionWithDelegate.Create(Model, Member.Name,
            CreatePropertyInjectionDelegate(LImportAttribute.Name));
          LInjection.Initialize(Member);
          Model.PropertyInjections.Add(LInjection);
        end else if Member is TRttiField then
        begin
          LInjection := TFieldInjectionWithDelegate.Create(Model, Member.Name,
            CreateFieldInjectionDelegate(LImportAttribute.Name));
          LInjection.Initialize(Member);
          Model.FieldInjections.Add(LInjection);
        end;
      end else
      begin
        if Member is TRttiProperty then
          Model.InjectProperty(Member.Name)
        else if Member is TRttiField then
          Model.InjectField(Member.Name);
      end;
    end;
  end;
end;

procedure TSpringContainer.ImportRtti;
var
  LType: TRttiType;
  LExportAttribute: ExportAttribute;
  LField: TRttiField;
  LProperty: TRttiProperty;
  LModel: TComponentModel;
begin
  FInterfaces := TDictionary<TGUID, TRttiInterfaceType>.Create();
  try
    for LType in GetRttiTypes do
    begin
      if LType is TRttiInterfaceType and LType.HasAttributeOfType<InheritedExportAttribute> then
      begin
        if not IsEqualGUID(TRttiInterfaceType(LType).GUID, TGUID.Empty) then
        begin
          FInterfaces.Add(TRttiInterfaceType(LType).GUID, TRttiInterfaceType(LType));
        end;
      end;
    end;

    for LType in GetRttiTypes do
    begin
      if LType is TRttiInstanceType and not LType.HasAttributeOfType<AbstractAttribute> then
      begin
        if LType.TryGetAttributeOfType<ExportAttribute>(LExportAttribute) then
        begin
          RegisterClass(LType);
        end;

        RegisterClassImplementingInterface(TRttiInstanceType(LType));
        RegisterClassInheritedExport(TRttiInstanceType(LType));

        if ComponentRegistry.HasComponent(LType.Handle) then
        begin
          LModel := ComponentRegistry.GetComponent(LType.Handle);

          for LProperty in LType.GetProperties do
          begin
            ImportMember(LModel, LProperty);
          end;

          for LField in LType.GetFields do
          begin
            ImportMember(LModel, LField);
          end;
        end;

        for LProperty in LType.GetProperties do
        begin
          // register types that got exported properties as singleton
          if LProperty.TryGetAttributeOfType<ExportAttribute>(LExportAttribute) then
          begin
            if not GetComponentRegistry.HasComponent(LType.Handle) then
            begin
              RegisterClass(LType);
            end;

            if LExportAttribute.Name = '' then
              FExportedProperties.Add(LProperty.Name, LProperty)
            else
              FExportedProperties.Add(LExportAttribute.Name, LProperty);
          end;
        end;
      end;
    end;
  finally
    FInterfaces.Free();
    Build();
  end;
end;

function TSpringContainer.RegisterClass(ClassType: TRttiType): TRegistration;
var
  LPartCreationPolicyAttribute: PartCreationPolicyAttribute;
begin
  Result := RegisterType(ClassType.Handle);
  if ClassType.TryGetAttributeOfType<PartCreationPolicyAttribute>(LPartCreationPolicyAttribute)
    and (LPartCreationPolicyAttribute.Policy = cpShared) then
  begin
    Result.AsSingleton;
  end;
end;

procedure TSpringContainer.RegisterClassImplementingInterface(ClassType: TRttiInstanceType);
var
  i: Integer;
  LInheritedExportAttribute: InheritedExportAttribute;
  LInterfaceTable: PInterfaceTable;
  LInterfaceType: TRttiInterfaceType;
  LParentType: TRttiInstanceType;
  LRegistration: TRegistration;
begin
  LParentType := ClassType;
  while Assigned(LParentType) do
  begin
    LInterfaceTable := LParentType.MetaclassType.GetInterfaceTable();
    if Assigned(LInterfaceTable) then
    begin
      for i := 0 to LInterfaceTable.EntryCount - 1 do
      begin
        if FInterfaces.TryGetValue(LInterfaceTable.Entries[i].IID, LInterfaceType) then
        begin
          if not ComponentRegistry.HasComponent(ClassType.Handle) then
          begin
            LRegistration := RegisterClass(ClassType);

            LInheritedExportAttribute := LInterfaceType.GetAttributeOfType<InheritedExportAttribute>;
            if LInheritedExportAttribute.Name = '' then
              LRegistration.Implements(LInterfaceType.Handle)
            else
              LRegistration.Implements(LInterfaceType.Handle, LInheritedExportAttribute.Name);
          end;
        end;
      end;
    end;
    LParentType := LParentType.BaseType;
  end;
end;

procedure TSpringContainer.RegisterClassInheritedExport(ClassType: TRttiInstanceType);
var
  LInheritedExportAttribute: InheritedExportAttribute;
  LParentType: TRttiInstanceType;
  LRegistration: TRegistration;
begin
  LParentType := ClassType.BaseType;
  while Assigned(LParentType) do
  begin
    if LParentType.HasAttributeOfType<InheritedExportAttribute> then
    begin
      if not ComponentRegistry.HasComponent(ClassType.Handle) then
      begin
        LRegistration := RegisterType(ClassType.Handle);

        LInheritedExportAttribute := LParentType.GetAttributeOfType<InheritedExportAttribute>;

        if LInheritedExportAttribute.Name = '' then
          LRegistration.Implements(LParentType.Handle)
        else
          LRegistration.Implements(LParentType.Handle, LInheritedExportAttribute.Name);
      end;
    end;
    LParentType := LParentType.BaseType;
  end;
end;

function TSpringContainer.Resolve(TypeInfo: PTypeInfo;
  const Name: string): TValue;
begin
  if Name = '' then
    Result := Resolve(TypeInfo)
  else
    Result := Resolve(Name);
end;

function TSpringContainer.ResolveAllLazy(LazyType: TRttiType): TArray<TValue>;
var
  Models: IEnumerable<TComponentModel>;
  Index: Integer;
  TempResult: TArray<TValue>;
begin
  Models := GetComponentRegistry.FindAll(LazyType.Handle);
  SetLength(TempResult, Models.Count);
  Index := 0;
  Models.ForEach(
    procedure(const Model: TComponentModel)
    begin
      if LazyType is TRttiInterfaceType then
      begin
        TempResult[Index] := TValue.From<ILazy<IInterface>>(ILazy<IInterface>(
          function: IInterface
          begin
            Result := Resolve(Model.GetServiceName(LazyType.Handle)).AsInterface;
          end));
      end;
      if LazyType is TRttiInstanceType then
      begin
        TempResult[Index] := TValue.From<ILazy<TObject>>(ILazy<TObject>(
          function: TObject
          begin
            Result := Resolve(Model.GetServiceName(LazyType.Handle)).AsObject;
          end));
      end;
      TValueData(TempResult[Index]).FTypeInfo := LazyType.Handle;
      Inc(Index);
    end);
  Result := TempResult;
  SetLength(TempResult, 0);
end;

function TSpringContainer.ResolveAllLazy<TServiceType>: TArray<ILazy<TServiceType>>;
begin
  Result := TArray<ILazy<TServiceType>>(ResolveAllLazy(GetRttiType(TypeInfo(TServiceType))));
end;

function TSpringContainer.ResolveLazy(LazyType: TRttiType): TValue;
var
  LazyContentType: TRttiType;
  Value: TValue;
begin
  LazyContentType := LazyType.GetGenericArguments[0];
  if LazyContentType is TRttiInterfaceType then
  begin
    Value := TValue.From<Lazy<IInterface>>(Lazy<IInterface>(
      function: IInterface
      begin
        Result := Resolve(LazyContentType.Handle).AsInterface;
      end));
  end;
  if LazyContentType is TRttiInstanceType then
  begin
    Value := TValue.From<Lazy<TObject>>(Lazy<TObject>(
      function: TObject
      begin
        Result := Resolve(LazyContentType.Handle).AsObject;
      end));
  end;
  TValueData(Value).FTypeInfo := LazyType.Handle;
  Result := Value;
end;

function TSpringContainer.ResolveLazy<T>: ILazy<T>;
begin
  Result := ResolveLazy(TypeInfo(T)).AsType<ILazy<T>>();
end;

{ TPropertyInjectionWithDelegate }

constructor TPropertyInjectionWithDelegate.Create(model: TComponentModel;
  const targetName: string; delegate: TFunc<TValue>);
begin
  inherited Create(model, targetName);
  fDelegate := delegate;
end;

procedure TPropertyInjectionWithDelegate.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  inherited DoInject(instance, [fDelegate()]);
end;

{ TFieldInjectionWithDelegate }

constructor TFieldInjectionWithDelegate.Create(model: TComponentModel;
  const targetName: string; delegate: TFunc<TValue>);
begin
  inherited Create(model, targetName);
  fDelegate := delegate;
end;

procedure TFieldInjectionWithDelegate.DoInject(instance: TObject;
  const arguments: array of TValue);
begin
  inherited DoInject(instance, [fDelegate()]);
end;

end.
