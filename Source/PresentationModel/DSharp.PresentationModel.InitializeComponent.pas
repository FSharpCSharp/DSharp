unit DSharp.PresentationModel.InitializeComponent;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Rtti,
  DSharp.Bindings,
  DSharp.Collections,
  DSharp.Core.Reflection,
  DSharp.Core.DataConversion,
  DSharp.Core.Framework,
  DSharp.Logging,
  DSharp.PresentationModel,
  DSharp.PresentationModel.BindingString,
  DSharp.PresentationModel.Extensions;

type
  EBindingArgumentException = class(EArgumentException);

  TBindableProperty = record
    Target: TComponent;
    TargetPropertyName: string;
    BindingString: string;
    constructor Create(const Target: TComponent;
      const Attribute: BindingAttribute);
  end;

  TInitializeComponent = record
  private
    class var FLog: ILog;
    class function GetBindingMode(const BindingString: IBindingString)
      : TBindingMode; static;
    class function GetValueConverter(const BindingString: IBindingString)
      : IValueConverter; static;
    class function GetSource(const BindingString: IBindingString;
      Target: TComponent): TObject; static;
    class function GetSourcePropertyName(const BindingString: IBindingString)
      : string; static;
    class function GetUpdateSourceTrigger(const BindingString: IBindingString)
      : TUpdateTrigger; static;
    class function GetValidatesOnDataErrors(const BindingString: IBindingString)
      : Boolean; static;
    class function FindBindableProperties(const Target: TComponent)
      : IList<TBindableProperty>; static;
    class function GetLog(): ILog; static;
    class procedure InitializeDesigntimeBindings(const Component
      : TComponent); static;
    class procedure InitializeRuntimeBindings(const Component
      : TComponent); static;
    class procedure ProcessBinding(const Target: TComponent;
      const TargetPropertyName, BindingString: string;
      const BindingGroup: TBindingGroup); static;
    class procedure ProcessValue(const Target: TComponent;
      const TargetPropertyName: string;
      const TargetPropertyValue: TValue); static;
    class property Log: ILog read GetLog;
  public
    ///	<summary>
    ///	  Initializes dependency properties declared on the component
    ///	</summary>
    class procedure Initialize(const Component: TComponent); static;
  end;

implementation

uses
  TypInfo;

resourcestring
  SElementNotFound = 'Element not found [%s]';
  SBindingStringIsInvalid = 'Binding string ''%s'' is invalid';
  SBindingStringIsNotSupported = 'Binding string ''%s'' is not supported';

  { TBindableProperty }

constructor TBindableProperty.Create(const Target: TComponent;
  const Attribute: BindingAttribute);
begin
  Self.Target := Target;
  Self.TargetPropertyName := Attribute.TargetPropertyName;
  Self.BindingString := Attribute.BindingString;
end;

class function TInitializeComponent.FindBindableProperties
  (const Target: TComponent): IList<TBindableProperty>;
var
  LAttribute: TCustomAttribute;
  LElement: TComponent;
  LField: TRttiField;
  LTargetType: TRttiType;
begin
  Result := TList<TBindableProperty>.Create;

  LTargetType := GetRttiType(Target.ClassType);

  // Scan Target for BindingAttribute
  for LAttribute in LTargetType.GetAttributes do
    if LAttribute is BindingAttribute then
      Result.Add(TBindableProperty.Create(Target,
        LAttribute as BindingAttribute));

  // Scan Target's fields for BindingAttribute
  for LField in LTargetType.GetFields do
    for LAttribute in LField.GetAttributes do
      if LAttribute is BindingAttribute then
      begin
        LElement := LField.GetValue(Target).AsType<TComponent>;
        Result.Add(TBindableProperty.Create(LElement,
          LAttribute as BindingAttribute));
      end;
end;

{ TInitializeComponent }

class procedure TInitializeComponent.Initialize(const Component: TComponent);
begin
  // Skip if component has already been initialized
  if Component.IsComponentInitialized then
    Exit;

  // Initialize designtime bindings (BindingAttribute)
  InitializeDesigntimeBindings(Component);

  // Initialize runtime bindings
  InitializeRuntimeBindings(Component);

  // Set flag that component has been initialized
  Component.IsComponentInitialized := True;
end;

class procedure TInitializeComponent.InitializeDesigntimeBindings
  (const Component: TComponent);
var
  LBindingGroup: TBindingGroup;
  LBindableProperty: TBindableProperty;
  LBindableProperties: IList<TBindableProperty>;
  LBindingString: string;
begin
  LBindableProperties := FindBindableProperties(Component);
  if LBindableProperties.Count > 0 then
  begin
    // Find or create a binding group associated with the Component
    LBindingGroup := FindBindingGroup(Component);
    if not Assigned(LBindingGroup) then
      LBindingGroup := TBindingGroup.Create(Component);

    // Process bindings declared with attributes
    for LBindableProperty in LBindableProperties do
    begin
      LBindingString := LBindableProperty.BindingString;
      if TBindingString.MatchesBasicBindingSyntax(LBindingString) then
      begin
        ProcessBinding(LBindableProperty.Target,
          LBindableProperty.TargetPropertyName, LBindingString, LBindingGroup);
      end
      else
      begin
        ProcessValue(LBindableProperty.Target,
          LBindableProperty.TargetPropertyName, LBindingString);
      end;
    end;
  end;
end;

class procedure TInitializeComponent.InitializeRuntimeBindings(const Component
  : TComponent);
var
  LTargetType: TRttiType;
  LDefineBindingsMethod: TRttiMethod;
  LDefinesBindings: IDefinesBindings;
begin
  // Invoke DefineBindings method on the view
  if Supports(Component, IDefinesBindings, LDefinesBindings) then
  begin
    LDefinesBindings.DefineBindings;
  end
  else
  begin
    LTargetType := GetRttiType(Component.ClassType);
    LDefineBindingsMethod := LTargetType.GetMethod('DefineBindings');
    if Assigned(LDefineBindingsMethod) and
      (LDefineBindingsMethod.ParameterCount = 0) then
    begin
      LDefineBindingsMethod.Invoke(Component, []);
    end;
  end;
end;

class function TInitializeComponent.GetBindingMode(const BindingString
  : IBindingString): TBindingMode;
const
  EnumsText: array [0 .. 4] of string = ('TwoWay', 'OneWay', 'OneTime',
    'OneWayToSource', 'Default');
  Enums: array [0 .. 4] of TBindingMode = (bmTwoWay, bmOneWay, bmOneTime,
    bmOneWayToSource, bmTwoWay);
var
  LIndex: Integer;
  LValue: string;
begin
  // Exit if value is not assigned
  if not BindingString.TryGetValue('Mode', LValue) then
    Exit(bmTwoWay);

  LIndex := IndexText(LValue, EnumsText);
  if LIndex >= 0 then
    Result := Enums[LIndex]
  else
    raise EBindingArgumentException.CreateFmt(SBindingStringIsInvalid,
      ['Mode']);
end;

class function TInitializeComponent.GetLog(): ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(TInitializeComponent));
  end;
  Result := FLog;
end;

class function TInitializeComponent.GetValidatesOnDataErrors(const BindingString
  : IBindingString): Boolean;
const
  EnumsText: array [0 .. 1] of string = ('True', 'False');
  Enums: array [0 .. 1] of Boolean = (True, False);
var
  LIndex: Integer;
  LValue: string;
begin
  // Exit if value is not assigned
  if not BindingString.TryGetValue('ValidatesOnDataErrors', LValue) then
    Exit(False);

  LIndex := IndexText(LValue, EnumsText);
  if LIndex >= 0 then
    Result := Enums[LIndex]
  else
    raise EBindingArgumentException.CreateFmt(SBindingStringIsInvalid,
      ['ValidatesOnDataErrors']);
end;

class function TInitializeComponent.GetValueConverter(const BindingString
  : IBindingString): IValueConverter;
var
  LValue: string;
  LType: TRttiType;
begin
  // Exit if value is not assigned
  if not BindingString.TryGetValue('Converter', LValue) then
    Exit(nil);

  // TODO: Expand Converter syntax with support for ConverterParameter
  if FindType(LValue, LType) then
  begin
    Supports(LType.AsInstance.MetaclassType.Create, IValueConverter, Result);
  end
  else
    raise EBindingArgumentException.CreateFmt(SBindingStringIsNotSupported,
      ['Converter']);
end;

class function TInitializeComponent.GetSource(const BindingString
  : IBindingString; Target: TComponent): TObject;
var
  LTarget: TComponent;
  LValue: string;
begin
  // Handle empty binding syntax
  if BindingString.IsEmpty then
    Exit(Target);

  if BindingString.TryGetValue('Source', LValue) then
    raise EBindingArgumentException.CreateFmt(SBindingStringIsNotSupported,
      ['Source']);

  if BindingString.TryGetValue('ElementName', LValue) then
  begin
    LTarget := Target;
    while Assigned(LTarget) do
    begin
      if LTarget.Name = LValue then
        Exit(Target);

      if Assigned(LTarget.FindComponent(LValue)) then
        Exit(LTarget.FindComponent(LValue));

      LTarget := TFramework.GetParent(LTarget);
    end;
    raise EBindingArgumentException.CreateFmt(SElementNotFound, [LValue]);
  end;

  if BindingString.TryGetValue('RelativeSource', LValue) then
    raise EBindingArgumentException.CreateFmt(SBindingStringIsNotSupported,
      ['RelativeSource']);

  // Get inherited data context
  Result := Target.DataContext;
  if not Assigned(Result) then
    Log.LogWarning('Could not resolve inherited DataContext for Target %s.',
      [Target.Describe()]);
end;

class function TInitializeComponent.GetSourcePropertyName(const BindingString
  : IBindingString): string;
var
  LValue: string;
begin
  // Exit if property is empty
  if BindingString.IsEmpty then
    Exit('');

  // Try to get source property from Path
  if BindingString.TryGetValue('Path', Result) then
    Exit(Result);

  // Try to get source property from any remaining value
  for LValue in BindingString do
    if Pos('=', LValue) = 0 then
      Exit(LValue);

  raise EBindingArgumentException.CreateFmt(SBindingStringIsInvalid, ['Path']);
end;

class function TInitializeComponent.GetUpdateSourceTrigger(const BindingString
  : IBindingString): TUpdateTrigger;
const
  EnumsText: array [0 .. 3] of string = ('PropertyChanged', 'LostFocus',
    'Explicit', 'Default');
  Enums: array [0 .. 3] of TUpdateTrigger = (utPropertyChanged, utLostFocus,
    utExplicit, utPropertyChanged);
var
  LIndex: Integer;
  LValue: string;
begin
  // Exit if property is empty
  if not BindingString.TryGetValue('UpdateSourceTrigger', LValue) then
    Exit(utPropertyChanged);

  LIndex := IndexText(LValue, EnumsText);
  if LIndex >= 0 then
    Result := Enums[LIndex]
  else
    raise EBindingArgumentException.CreateFmt(SBindingStringIsInvalid,
      ['UpdateSourceTrigger']);
end;

class procedure TInitializeComponent.ProcessBinding(const Target: TComponent;
  const TargetPropertyName, BindingString: string;
  const BindingGroup: TBindingGroup);
var
  LBinding: TBinding;
  LBindingMode: TBindingMode;
  LBindingModeDescription: string;
  LBindingString: IBindingString;
  LSource: TObject;
  LSourcePropertyName: string;
  LSourceUpdateTrigger: TUpdateTrigger;
  LValidatesOnDataErrors: Boolean;
  LValueConverter: IValueConverter;
begin
  // Parse binding string
  LBindingString := TBindingString.Create(BindingString);

  // Get BindingMode
  LBindingMode := GetBindingMode(LBindingString);

  // Get Converter
  LValueConverter := GetValueConverter(LBindingString);

  // Get Path
  LSourcePropertyName := GetSourcePropertyName(LBindingString);

  // Get Source
  LSource := GetSource(LBindingString, Target);

  // Get UpdateSourceTrigger
  LSourceUpdateTrigger := GetUpdateSourceTrigger(LBindingString);

  // Get ValidatesOnDataErrors
  LValidatesOnDataErrors := GetValidatesOnDataErrors(LBindingString);

  // Check
  if not Assigned(LSource) then
  begin
    Log.LogError
      ('Is your Target supported by `TPresentationFramework.DoGetParent()?`. Could not find Source for binding "%s" on Target %s',
      [BindingString, Target.Describe()]);
    Assert(False, 'Source is not assigned!');
  end;

  // Create binding from source to target
  LBinding := BindingGroup.AddBinding(LSource, LSourcePropertyName, Target,
    TargetPropertyName, LBindingMode, LValueConverter);
  LBinding.SourceUpdateTrigger := LSourceUpdateTrigger;
  LBinding.ValidatesOnDataErrors := LValidatesOnDataErrors;

  LBindingModeDescription := GetEnumName(TypeInfo(TBindingMode),
    Ord(LBindingMode));
  Log.LogMessage
    ('%s Binding Applied: property %s of %s to property %s of Element %s.',
    [LBindingModeDescription, LSourcePropertyName, LSource.Describe(),
    TargetPropertyName, Target.Describe()])
  { TODO -o##jwp -cEnhance : Add warnings for binding when the Source or Target cannot be evaluated. }
end;

class procedure TInitializeComponent.ProcessValue(const Target: TComponent;
  const TargetPropertyName: string; const TargetPropertyValue: TValue);
var
  LConvertedValue: TValue;
  LProperties: TArray<TDependencyProperty>;
  LProperty: TRttiProperty;
begin
  // Find property by name
  LProperty := Target.GetProperty(TargetPropertyName);
  if Assigned(LProperty) then
  begin
    // Convert value of type string to PropertyType
    TargetPropertyValue.TryConvert(LProperty.PropertyType.Handle,
      LConvertedValue);
    // Set value
    LProperty.SetValue(Target, LConvertedValue);
    Exit;
  end;

  // Find dependency property by name
  LProperties := TDependencyProperty.FindByName(TargetPropertyName);
  if Length(LProperties) = 1 then
  begin
    // Convert value of type string to PropertyType
    TargetPropertyValue.TryConvert(LProperties[0].PropertyType,
      LConvertedValue);
    // Set value
    LProperties[0].SetValue(Target, LConvertedValue);
    Exit;
  end;

  raise Exception.CreateFmt(SElementNotFound, [TargetPropertyName]);
end;

end.
