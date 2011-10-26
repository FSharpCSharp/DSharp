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

unit DSharp.ComponentModel.Composition.Container deprecated;

interface

uses
  Classes,
  DSharp.ComponentModel.Composition,
  DSharp.ComponentModel.Composition.Primitives,
  DSharp.Core.Lazy,
  Generics.Collections,
  Rtti,
  SysUtils,
  TypInfo;

type
  ILazyEvaluator = interface
  ['{12956345-B037-484D-88D1-FF7706184C20}']
    function Evaluate: TObject;
  end;

  TCompositionContainer = class(TInterfacedObject, IServiceLocator)
  private

    type
      TEvaluator = class(TInterfacedObject, ILazyEvaluator)
      private
        FContainer: TCompositionContainer;
        FInstanceClass: TClass;
        FName: string;
        FOwned: Boolean;
        FPolicy: TCreationPolicy;
      public
        constructor Create(Container: TCompositionContainer; const Name: string;
          InstanceClass: TClass; Policy: TCreationPolicy; Owned: Boolean);
        function Evaluate: TObject;
      end;

  private
    FCatalog: TBaseCatalog;
    FContext: TRttiContext;
    FObjects: TObjectDictionary<string, TObject>;
    FOwnedObjects: TObjectList<TObject>;
    FPendingCreation: TStringList;
    function CheckAndCreate(const Name: string; InstanceClass: TClass;
      AddToContainer: Boolean; OwnsObject: Boolean): TObject;
    function CreateLazy(LazyType: TRttiType; LazyContentType: TRttiType;
      ALazyEvaluator: ILazyEvaluator; MetaData: TExportMetaData = nil): TValue;
    function GetRepositoryName(AType: TClass): string; overload;
    function Instantiate(AType: TClass; OwnsObject: Boolean): TObject;
  protected
    function Factory(const Name: string; InstanceClass: TClass;
      Policy: TCreationPolicy; Owned: Boolean; out Value: TObject): Boolean;
  public
    function GetExport(const Name: string; TypeInfo: PTypeInfo): TValue; overload;
    function GetExportName(const Name: string; TypeInfo: PTypeInfo): string;
    function GetExports(const Name: string; TypeInfo: PTypeInfo): TValue; overload;
  protected
    function GetLazyContentType(RttiType: TRttiType): TRttiType;
    function TryGetExport(const Name: string; TypeInfo: PTypeInfo; out Value: TValue): Boolean;
    function TryGetExports(const Name: string; TypeInfo: PTypeInfo; out OutValues: TValue): Boolean;
  public
    constructor Create(Catalog: TBaseCatalog);
    destructor Destroy; override;

    function GetExport<T>: T; overload;
    function GetExports<T>: T; overload;

    function GetExport(const Name: string): TObject; overload;
    function GetExport<T>(const Name: string): T; overload;
    function GetExports<T>(const Name: string): T; overload;

    function Resolve(TypeInfo: PTypeInfo; const Name: string): TValue;
    procedure SatisfyImportsOnce(Instance: TObject);
  end;

  ELazyException = class(Exception);

implementation

uses
  DSharp.Core.Reflection;

type
  TLazy<T> = class(TInterfacedObject, ILazy<T>)
  private
    FInternalValue: T;
    FLazyEvaluator: ILazyEvaluator;
    FMetaData: TExportMetaData;
    FTypeInfo: PTypeInfo;
    function GetMetaData: TObject;
    function Invoke: T;
  public
    constructor Create(ALazyEvaluator: ILazyEvaluator; TypeInfo: PTypeInfo;
      MetaData: TExportMetaData = nil);

    function IsValueCreated: Boolean;
    property MetaData: TObject read GetMetaData;
    property Value: T read Invoke;
  end;

{ TCompositionContainer }

function TCompositionContainer.CheckAndCreate(const Name: string; InstanceClass: TClass; AddToContainer: Boolean; OwnsObject: Boolean): TObject;
var
  LFirstExport: Boolean;
  i: Integer;
begin
  Result := nil;
  LFirstExport := not Assigned(FPendingCreation);
  if LFirstExport then
  begin
    FPendingCreation := TStringList.Create;
  end;
  try
    if FPendingCreation.IndexOf(Name) >= 0 then
    begin
      raise ECompositionException.Create('Could not instantiate object due to circular dependency.');
    end;
    i := FPendingCreation.Add(Name);
    try
      Result := Instantiate(InstanceClass, OwnsObject);
      if not Assigned(Result) then
      begin
        raise ECompositionException.Create('Could not instantiate object.');
      end;
      if AddToContainer then
      begin
        FObjects.Add(Name, Result);
      end;
      SatisfyImportsOnce(Result);
    finally
      FPendingCreation.Delete(i);
    end;
  finally
    if LFirstExport then
    begin
      FreeAndNil(FPendingCreation);
    end;
  end;
end;

constructor TCompositionContainer.Create(Catalog: TBaseCatalog);
begin
  FCatalog := Catalog;
  FObjects := TObjectDictionary<string, TObject>.Create([]);
  FOwnedObjects := TObjectList<TObject>.Create;
  FContext := TRttiContext.Create;
end;

function TCompositionContainer.CreateLazy(LazyType: TRttiType; LazyContentType: TRttiType;
  ALazyEvaluator: ILazyEvaluator; MetaData: TExportMetaData): TValue;
var
  LValue: TValue;
  LLazyInterface: IInterface;
  LLazyObject: IInterface;
begin
  // a little bit of binary trickery

  if LazyContentType is TRttiInterfaceType then
  begin
    LLazyInterface := ILazy<IInterface>(TLazy<IInterface>.Create(
      ALazyEvaluator, LazyContentType.Handle, MetaData));
    LValue := TValue.From<IInterface>(LLazyInterface);
  end;

  if LazyContentType is TRttiInstanceType then
  begin
    LLazyObject := ILazy<TObject>(TLazy<TObject>.Create(
      ALazyEvaluator, LazyContentType.Handle, MetaData));
    LValue := TValue.From<IInterface>(LLazyObject);
  end;

  TValueData(LValue).FTypeInfo := LazyType.Handle;
  Result := LValue;
end;

destructor TCompositionContainer.Destroy;
begin
  FOwnedObjects.Free;
  FObjects.Free;
  inherited;
end;

function TCompositionContainer.Factory(const Name: string; InstanceClass: TClass;
  Policy: TCreationPolicy; Owned: Boolean; out Value: TObject): Boolean;
var
  LRepositoryName: string;
begin
  Value := nil;
  Result := False;
  case Policy of
    cpShared:
      begin
        LRepositoryName := GetRepositoryName(InstanceClass);

        Result := FObjects.TryGetValue(LRepositoryName, Value);
        if not Result then
        begin
          Value := CheckAndCreate(LRepositoryName, InstanceClass, True, Owned);
          Result := True;
        end;
      end;
    cpNonShared:
    begin
      Value := CheckAndCreate(Name, InstanceClass, False, Owned);
      Result := True;
    end;
  end;
end;

function TCompositionContainer.GetExport(const Name: string; TypeInfo: PTypeInfo): TValue;
var
  LValue: TValue;
begin
  if TryGetExport(Name, TypeInfo, Result) then
  begin
    // Workaround for RTL bug. We want to assign the TValue in the correct type
    LValue := Result;
    Result.TryCast(TypeInfo, LValue);
    Result := LValue;
  end
  else
  begin
    raise ECompositionException.Create('No export is defined for ' +
      GetExportName(Name, TypeInfo));
  end;
end;

function TCompositionContainer.TryGetExport(const Name: string; TypeInfo: PTypeInfo; out Value: TValue): Boolean;
var
  LInfos: TExportInfoList;
  LOwned: Boolean;
  LFinalName: string;
  LTargetType, LLazyContentType: TRttiType;
  LObject: TObject;
begin
  Value := nil;
  Result := False;

  LFinalName := Name;

  LTargetType := FContext.GetType(TypeInfo);

  LLazyContentType := GetLazyContentType(LTargetType);
  if Assigned(LLazyContentType) then
  begin
    if LFinalName = '' then
    begin
      LFinalName := LLazyContentType.QualifiedName;
    end;

    LOwned := LLazyContentType.TypeKind <> tkInterface;
  end
  else
  begin
    if LFinalName = '' then
    begin
      LFinalName := LTargetType.QualifiedName;
    end;

    LOwned := LTargetType.TypeKind <> tkInterface;
  end;

  if FCatalog.CanResolve(LFinalName) then
  begin
    LInfos := FCatalog.GetExports(LFinalName);
    if LInfos.Count <> 1 then
    begin
      raise ECompositionException.Create('There are multiple exports but a single import was requested.');
    end;

    if Assigned(LInfos[0].RttiProperty) then
      LOwned := True;

    if Assigned(LLazyContentType) then
    begin
      Value := CreateLazy(LTargetType, LLazyContentType,
        TEvaluator.Create(Self, LFinalName, LInfos[0].InstanceClass,
        LInfos[0].Policy, LOwned), LInfos[0].MetaData);
      Result := True;
    end
    else
    begin
      Result := Factory(LFinalName, LInfos[0].InstanceClass,
        LInfos[0].Policy, LOwned, LObject);
      // handle exported property
      if LInfos[0].RttiProperty <> nil then
      begin
        Result := Result and LInfos[0].RttiProperty.IsReadable;
        if Result then
        begin
          Value := LInfos[0].RttiProperty.GetValue(LObject);
        end;
      end
      else
      begin
        if Result then
        begin
          Value := LObject;
        end
        else
        begin
          Value := nil;
        end;
      end;
    end;
  end;
end;

function TCompositionContainer.TryGetExports(const Name: string;
  TypeInfo: PTypeInfo; out OutValues: TValue): Boolean;
var
  LInfos: TExportInfoList;
  LInfo: TExportInfo;
  LObject: TObject;
  i: Integer;
  LValues: array of TValue;
  LValue: TValue;
  LElementType: TRttiType;
  LOwned: Boolean;
  LLazyContentType: TRttiType;
  LFinalName: string;
begin
  LValues := nil;
  Result := False;

  LElementType := (FContext.GetType(TypeInfo) as TRttiDynamicArrayType).ElementType;

  LLazyContentType := GetLazyContentType(LElementType);
  if Assigned(LLazyContentType) then
  begin
    if LFinalName = '' then
    begin
      LFinalName := LLazyContentType.QualifiedName;
    end;

    LOwned := LLazyContentType.TypeKind <> tkInterface;
  end
  else
  begin
    if LFinalName = '' then
    begin
      LFinalName := LElementType.QualifiedName;
    end;

    LOwned := LElementType.TypeKind <> tkInterface;
  end;

  if FCatalog.CanResolve(LFinalName) then
  begin
    LInfos := FCatalog.GetExports(LFinalName);
    if LInfos.Count = 0 then
    begin
      raise ECompositionException.Create('Resolving failed');
    end;

    SetLength(LValues, LInfos.Count);

    if Assigned(LLazyContentType) then
    begin
      for i := 0 to LInfos.Count - 1 do
      begin
        LInfo := LInfos[i];
        LValues[i] := CreateLazy(LElementType, LLazyContentType,
          TEvaluator.Create(Self, LFinalName, LInfo.InstanceClass, LInfo.Policy,
          LOwned), LInfo.MetaData);
      end;
      Result := True;
    end
    else
    begin
      Result := True;
      for i := 0 to LInfos.Count - 1 do
      begin
        LInfo := LInfos[i];
        LObject := nil;

        Result := Result
          and Factory(LFinalName, LInfo.InstanceClass, LInfo.Policy, LOwned, LObject);
        // Workaround for RTL bug. We want to assign the TValue in the correct type
        LValues[i] := LObject;
        LValue := LValues[i];
        LValues[i].TryCast(LElementType.Handle, LValue);
        LValues[i] := LValue;
      end;
    end;
  end;

  OutValues := TValue.FromArray(TypeInfo, LValues);
end;

function TCompositionContainer.GetExport<T>: T;
begin
  Result := GetExport<T>(FContext.GetType(TypeInfo(T)).QualifiedName);
end;

function TCompositionContainer.GetExport(const Name: string): TObject;
begin
  Result := GetExport<TObject>(Name);
end;

function TCompositionContainer.GetExport<T>(const Name: string): T;
var
  LValue: TValue;
begin
  LValue := GetExport(Name, TypeInfo(T));
  Result := LValue.AsType<T>;
end;

function TCompositionContainer.GetExportName(const Name: string; TypeInfo: PTypeInfo): string;
var
  LType, LLazyContentType: TRttiType;
begin
  if Name <> '' then
  begin
    Result := Name;
  end
  else
  begin
    LType := FContext.GetType(TypeInfo);

    if LType is TRttiDynamicArrayType then
    begin
      LType := TRttiDynamicArrayType(LType).ElementType;
    end;

    LLazyContentType := GetLazyContentType(LType);
    if Assigned(LLazyContentType) then
    begin
      LType := LLazyContentType;
    end;

    Result := LType.QualifiedName;
  end;
end;

function TCompositionContainer.GetLazyContentType(RttiType: TRttiType): TRttiType;
begin
  if RttiType.IsGenericTypeOf('ILazy') or RttiType.IsGenericTypeOf('TFunc') then
  begin
    Result := RttiType.GetGenericArguments()[0];
    if not Assigned(Result) then
    begin
      raise ECompositionException.Create('Type parameter for ' + RttiType.Name + ' could not be found.');
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

function TCompositionContainer.GetExports(const Name: string; TypeInfo: PTypeInfo): TValue;
begin
  if not TryGetExports(Name, TypeInfo, Result) then
  begin
    raise ECompositionException.Create('No export is defined for ' + GetExportName(Name, TypeInfo));
  end;
end;

function TCompositionContainer.GetExports<T>: T;
begin
  Result := GetExports<T>(FContext.GetType(TypeInfo(T)).QualifiedName);
end;

function TCompositionContainer.GetExports<T>(const Name: string): T;
var
  LValue: TValue;
begin
  if not (FContext.Create.GetType(TypeInfo(T)) is TRttiDynamicArrayType) then
  begin
    raise ECompositionException.Create('GetExports<T> must supply dynamic array type.');
  end;
  LValue := GetExports(Name, TypeInfo(T));
  Result := LValue.AsType<T>;
end;

constructor TCompositionContainer.TEvaluator.Create(Container: TCompositionContainer;
  const Name: string; InstanceClass: TClass; Policy: TCreationPolicy; Owned: Boolean);
begin
  FContainer := Container;
  FName := Name;
  FInstanceClass := InstanceClass;
  FPolicy := Policy;
  FOwned := Owned;
end;

function TCompositionContainer.TEvaluator.Evaluate: TObject;
begin
  if not FContainer.Factory(FName, FInstanceClass, FPolicy, FOwned, Result) then
  begin
    raise ELazyException.Create('Failed lazy evaluating...');
  end;
end;

function TCompositionContainer.GetRepositoryName(AType: TClass): string;
begin
  Result := FContext.Create.GetType(AType).QualifiedName;
end;

function TCompositionContainer.Instantiate(AType: TClass; OwnsObject: Boolean): TObject;

  function HasImportingConstructor(Method: TRttiMethod): Boolean;
  var
    LAttribute: TCustomAttribute;
  begin
    for LAttribute in Method.GetAttributes do
    begin
      if LAttribute is ImportingConstructorAttribute then
      begin
        Exit(True);
      end;
    end;
    Result := False;
  end;

  function IsValidConstructor(RttiType: TRttiType; Method: TRttiMethod): Boolean;
  var
    LAnotherMethod: TRttiMethod;
  begin
    if not Method.IsConstructor then
    begin
      Exit(False);
    end;

    if Method.Parent.AsInstance.MetaclassType <> TObject then
    begin
      Exit(True);
    end;

    // Method is TObject.Create, check if there are other constructors
    for LAnotherMethod in RttiType.GetMethods do
    begin
      if LAnotherMethod.IsConstructor and (LAnotherMethod.Visibility = mvPublic) then
      begin
        if LAnotherMethod.Parent.AsInstance.MetaclassType <> TObject then
        begin
          Exit(False);
        end;
      end;
    end;

    Result := True;
  end;

var
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
  LType: TRttiType;
  LParamValues: array of TValue;
  i: Integer;
begin
  LType := FContext.Create.GetType(AType);
  Result := nil;

  for LMethod in LType.GetMethods do
  begin
    if LMethod.IsConstructor and (LMethod.Visibility = mvPublic) then
    begin
      LParams := LMethod.GetParameters;

      if (Length(LParams) = 0) or HasImportingConstructor(LMethod) then
      begin
        if (Length(LParams) = 0) and IsValidConstructor(LType, LMethod) then
        begin
          Result := LMethod.Invoke(LType.AsInstance.MetaclassType, []).AsObject;
          Break;
        end;

        if Length(LParams) > 0 then
        begin
          SetLength(LParamValues, Length(LParams));
          for i := 0 to Length(LParams) - 1 do
          begin
            LParamValues[i] := GetExport('', LParams[i].ParamType.Handle);
          end;
          Result := LMethod.Invoke(LType.AsInstance.MetaclassType, LParamValues).AsObject;
          Break;
        end;
      end;
    end;
  end;

  if (Result <> nil) and OwnsObject then
  begin
    FOwnedObjects.Add(Result);
  end;
end;

function TCompositionContainer.Resolve(TypeInfo: PTypeInfo; const Name: string): TValue;
begin
  Result := GetExport(Name, TypeInfo);
end;

procedure TCompositionContainer.SatisfyImportsOnce(Instance: TObject);
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LAttribute: TCustomAttribute;
  LName: string;
begin
  LType := FContext.GetType(Instance.ClassType);

  for LProperty in LType.GetProperties do
  begin
    for LAttribute in LProperty.GetAttributes do
    begin
      if LAttribute is ImportAttribute then
      begin
        if not LProperty.IsWritable then
        begin
          raise ECompositionException.Create('Property is not writable.');
        end;
        LName := ImportAttribute(LAttribute).Name;
        LProperty.SetValue(Instance, GetExport(LName, LProperty.PropertyType.Handle));
      end;
      if LAttribute is ImportManyAttribute then
      begin
        if not LProperty.IsWritable then
        begin
          raise ECompositionException.Create('Property is not writable.');
        end;
        if not (LProperty.PropertyType is TRttiDynamicArrayType) then
        begin
          raise ECompositionException.Create('Only dynamic arrays can be written to.');
        end;

        LProperty.SetValue(Instance, GetExports(ImportAttribute(LAttribute).Name,
          TRttiDynamicArrayType(LProperty.PropertyType).Handle));
      end;
    end;
  end;
end;

{ TLazy<T> }

function TLazy<T>.IsValueCreated: Boolean;
begin
  if SizeOf(FInternalValue) <> SizeOf(Pointer) then
  begin
    raise Exception.Create('Invalid internal value type');
  end;
  Result := Cardinal(Pointer(@FInternalValue)^) <> 0;
end;

constructor TLazy<T>.Create(ALazyEvaluator: ILazyEvaluator;
  TypeInfo: PTypeInfo; MetaData: TExportMetaData);
begin
  FLazyEvaluator := ALazyEvaluator;
  FTypeInfo := TypeInfo;


  FMetaData := MetaData;
  if SizeOf(FInternalValue) <> SizeOf(Pointer) then
  begin
    raise Exception.Create('Invalid internal value type');
  end;
end;

function TLazy<T>.GetMetaData: TObject;//TExportMetaData;
begin
  Result := FMetaData;
end;

function TLazy<T>.Invoke: T;
var
  LValue: TObject;
  LInterfaceValue: IInterface;
  LType: TRttiType;
begin
  if not IsValueCreated then
  begin
    if not Assigned(FLazyEvaluator) then
    begin
      raise ELazyException.Create('Could not lazy evaluate.');
    end;
    LValue := FLazyEvaluator.Evaluate;
    LType := TRttiContext.Create.GetType(FTypeInfo);
    if LType is TRttiInterfaceType then
    begin
      if not Supports(LValue, TRttiInterfaceType(LType).GUID, LInterfaceValue) then
      begin
        raise ELazyException.Create('Could not lazy evaluate.' + GUIDToString(TRttiInterfaceType(LType).GUID));
      end;
      LInterfaceValue._AddRef;
      Move(LInterfaceValue, FInternalValue, SizeOf(LInterfaceValue));
    end
    else
    begin
      if LType is TRttiInstanceType then
      begin
        FInternalValue := T(LValue)
      end
      else
      begin
        raise ELazyException.Create('Could not lazy evaluate.');
      end;
    end;
    if not IsValueCreated then
    begin
      raise ELazyException.Create('Could not lazy evaluate.');
    end;
  end;
  Result := FInternalValue;
end;

end.
