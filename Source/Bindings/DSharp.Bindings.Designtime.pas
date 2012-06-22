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

unit DSharp.Bindings.Designtime;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  DSharp.Bindings,
  Generics.Collections,
  TypInfo;

type
  TBindingSelectionEditor = class(TSelectionEditor, ISelectionPropertyFilter)
  public
    procedure FilterProperties(const ASelection: IDesignerSelections;
      const ASelectionProperties: IInterfaceList);
  end;

  TBindingGroupComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TBindingProperty = class(TClassProperty)
  private
    function FilterFunc(const ATestEditor: IProperty): Boolean;
  protected
    function GetIsDefault: Boolean; override;
  public
    constructor Create(const ADesigner: IDesigner; AComponent: TComponent); reintroduce;
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TSourceProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TSourcePropertyNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TTargetProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TTargetPropertyNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TSupportedClasses = class(TDictionary<TClass, string>)
  protected
    procedure KeyNotify(const Key: TClass; Action: TCollectionNotification); override;
  end;

procedure Register;

var
  SupportedClasses: TSupportedClasses;

implementation

uses
  ColnEdit,
  Consts,
  DSharp.Core.Reflection,
  RTLConsts,
  Rtti,
  StrUtils,
  SysUtils;

procedure Register;
begin
  RegisterComponents('Data binding', [TBindingGroup]);
  RegisterComponentEditor(TBindingGroup, TBindingGroupComponentEditor);
  RegisterPropertyEditor(TypeInfo(TObject), TBinding, 'Source', TSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TBinding, 'SourcePropertyName', TSourcePropertyNameProperty);
  RegisterPropertyEditor(TypeInfo(TObject), TBinding, 'Target', TTargetProperty);
  RegisterPropertyEditor(TypeInfo(string), TBinding, 'TargetPropertyName', TTargetPropertyNameProperty);
end;

function FindSupportedClass(AComponent: TPersistent): TClass;
var
  LSupportedClass: TClass;
begin
  if SupportedClasses.ContainsKey(AComponent.ClassType) then
  begin
    Result := AComponent.ClassType;
  end
  else
  begin
    Result := nil;
    for LSupportedClass in SupportedClasses.Keys do
    begin
      if AComponent is LSupportedClass then
      begin
        Result := LSupportedClass;
        Break;
      end;
    end;
  end;
end;

function SupportsBinding(AComponent: TPersistent): Boolean;
begin
  Result := Assigned(FindSupportedClass(AComponent));
end;

function GetTargetPropertyName(AComponent: TPersistent): string;
begin
  Result := SupportedClasses[FindSupportedClass(AComponent)];
end;

type
  TComponentHelper = class helper for TComponent
  private
    function GetBinding: TBinding;
  published
    property Binding: TBinding read GetBinding;
  end;

{ TComponentHelper }

function TComponentHelper.GetBinding: TBinding;
begin
  Result := FindBindingGroup(Self).GetBindingForTarget(Self);
end;

{ TBindingSelectionEditor }

procedure TBindingSelectionEditor.FilterProperties(
  const ASelection: IDesignerSelections;
  const ASelectionProperties: IInterfaceList);
var
  i: Integer;
  LBindingGroup: TBindingGroup;
  LProperty: TBindingProperty;
begin
  if ASelection.Count = 1 then
  begin
    if SupportsBinding(ASelection[0]) then
    begin
      LBindingGroup := FindBindingGroup(ASelection[0]);
      if Assigned(LBindingGroup) then
      begin
        with TComponent(ASelection[0]).Binding do
        begin
          if TargetPropertyName = '' then
          begin
            TargetPropertyName := GetTargetPropertyName(ASelection[0]);
          end;
        end;
        LProperty := TBindingProperty.Create(Designer, TComponent(ASelection[0]));
        for i := 0 to ASelectionProperties.Count - 1 do
        begin
          if Supports(ASelectionProperties[i], IProperty)
            and ((ASelectionProperties[i] as IProperty).GetName > 'Binding') then
          begin
            Break;
          end;
        end;
        ASelectionProperties.Insert(i, LProperty);
      end;
    end;
  end;
end;

{ TBindingGroupComponentEditor }

procedure TBindingGroupComponentEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
    (Component as TBindingGroup).Bindings, 'Bindings');
end;

function TBindingGroupComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Bindings Editor...';
  end;
end;

function TBindingGroupComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBindingProperty }

constructor TBindingProperty.Create(const ADesigner: IDesigner; AComponent: TComponent);
begin
  inherited Create(ADesigner, 1);
  SetPropEntry(0, AComponent, TypInfo.GetPropInfo(TypeInfo(TComponentHelper), 'Binding'));
end;

function TBindingProperty.FilterFunc(const ATestEditor: IProperty): Boolean;
begin
  Result := not SameText(ATestEditor.GetName(), 'Target')
    and not SameText(ATestEditor.GetName(), 'TargetPropertyName');
end;

function TBindingProperty.GetIsDefault: Boolean;
begin
  Result := True;
end;

procedure TBindingProperty.GetProperties(Proc: TGetPropProc);
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  Components.Add(TComponent(GetComponent(0)).Binding);
  GetComponentProperties(Components, tkProperties, Designer, Proc, FilterFunc);
end;

{ TSourceProperty }

procedure TSourceProperty.GetValues(Proc: TGetStrProc);
begin
  inherited;
  Proc(Designer.Root.Name);
end;

procedure TSourceProperty.SetValue(const Value: string);
var
  LBinding: TBinding;
  LObject: TObject;
begin
  LBinding := TBinding(GetComponent(0));

  if Value = '' then
  begin
    LObject := nil;
  end
  else
  begin
    LObject := Designer.GetComponent(Value);
    if not (LObject is GetTypeData(GetPropType)^.ClassType) then
      raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(NativeInt(LObject));

  if Assigned(LBinding.SourceProperty)
    and Assigned(LBinding.SourceProperty.Member) then
  begin
    if not LBinding.SourceProperty.Member.IsWritable then
    begin
      LBinding.BindingMode := bmOneWay;
    end;
  end
  else
  begin
    LBinding.SourcePropertyName := '';
  end;
end;

{ TSourcePropertyNameProperty }

function TSourcePropertyNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited + [paValueList];
end;

procedure TSourcePropertyNameProperty.GetValues(Proc: TGetStrProc);
var
  LProperty: TRttiProperty;
  LBinding: TBinding;
begin
  LBinding := TBinding(GetComponent(0));
  if Assigned(LBinding.Source) then
  begin
    for LProperty in LBinding.Source.GetProperties do
    begin
      if LProperty.PropertyType.TypeKind <> tkMethod then
      begin
        Proc(LProperty.Name);
      end;
    end;
  end;
end;

procedure TSourcePropertyNameProperty.SetValue(const Value: string);
var
  LBinding: TBinding;
begin
  inherited;

  LBinding := TBinding(GetComponent(0));

  if Assigned(LBinding.SourceProperty)
    and Assigned(LBinding.SourceProperty.Member) then
  begin
    if not LBinding.SourceProperty.Member.IsWritable
      and not (LBinding.BindingMode in [bmOneWay, bmOneTime]) then
    begin
      LBinding.BindingMode := bmOneWay;
    end;
  end;

  Modified;
end;

{ TTargetProperty }

procedure TTargetProperty.GetValues(Proc: TGetStrProc);
begin
  inherited;
  Proc(Designer.Root.Name);
end;

procedure TTargetProperty.SetValue(const Value: string);
var
  LBinding: TBinding;
  LObject: TObject;
begin
  LBinding := TBinding(GetComponent(0));

  if Value = '' then
  begin
    LObject := nil;
  end
  else
  begin
    LObject := Designer.GetComponent(Value);
    if not (LObject is GetTypeData(GetPropType)^.ClassType) then
      raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetOrdValue(NativeInt(LObject));

  if Assigned(LBinding.TargetProperty)
    and Assigned(LBinding.TargetProperty.Member) then
  begin
    if not LBinding.TargetProperty.Member.IsWritable
      and not (LBinding.BindingMode in [bmOneWayToSource]) then
    begin
      LBinding.BindingMode := bmOneWayToSource;
    end;
  end;
end;

{ TTargetPropertyNameProperty }

function TTargetPropertyNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited + [paValueList];
end;

procedure TTargetPropertyNameProperty.GetValues(Proc: TGetStrProc);
var
  LProperty: TRttiProperty;
  LBinding: TBinding;
begin
  LBinding := TBinding(GetComponent(0));
  if Assigned(LBinding.Target) then
  begin
    for LProperty in LBinding.Target.GetProperties do
    begin
      if LProperty.PropertyType.TypeKind <> tkMethod then
      begin
        Proc(LProperty.Name);
      end;
    end;
  end;
end;

procedure TTargetPropertyNameProperty.SetValue(const Value: string);
var
  LBinding: TBinding;
begin
  inherited;

  LBinding := TBinding(GetComponent(0));

  if Assigned(LBinding.TargetProperty)
    and Assigned(LBinding.TargetProperty.Member) then
  begin
    if not LBinding.TargetProperty.Member.IsWritable
      and not (LBinding.BindingMode in [bmOneWayToSource]) then
    begin
      LBinding.BindingMode := bmOneWayToSource;
    end;
  end;

  Modified;
end;

{ TSupportedClasses }

procedure TSupportedClasses.KeyNotify(const Key: TClass;
  Action: TCollectionNotification);
begin
  inherited;
  case Action of
    cnAdded: RegisterSelectionEditor(Key, TBindingSelectionEditor);
  end;
end;

initialization
  SupportedClasses := TSupportedClasses.Create();

finalization
  SupportedClasses.Free();

end.
