unit DSharp.PresentationModel.ConventionManager;

interface

uses
  Classes,
  SysUtils,
  Spring.Reflection,
  DSharp.Bindings,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ElementConvention,
  Generics.Collections,
  Rtti;

type
  TApplyValidationFunc = reference to procedure(Binding: TBinding;
    ViewModel: TObject; PropertyName: string);

  ///	<summary>
  ///	  Used to configure the conventions used by the framework to apply
  ///	  bindings and create actions.
  ///	</summary>
  ConventionManager = record
  private
  class var
    FConventions: TDictionary<TClass, TElementConvention>;
    FApplyValidation: TApplyValidationFunc;
  public
    class constructor Create;
    class destructor Destroy;

    class function AddElementConvention<T: class>(APropertyName: string;
      AEventName: string): TElementConvention; static;
    class procedure ConfigureSelectedItem(AViewModel: TObject;
      APropertyName: string; AViewElement: TComponent;
      ASelectedItemPropertyName: string); static;
    class function GetElementConvention(AElementType: TClass)
      : TElementConvention; static;
    class function SetBinding(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType;
      AConvention: TElementConvention): Boolean; overload; static;
    class function SetBinding(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ATargetPropertyName: string): Boolean;
      overload; static;

    ///	<summary>
    ///	  Determines whether or not and what type of validation to enable on
    ///	  the binding.
    ///	</summary>
    class property ApplyValidation: TApplyValidationFunc read FApplyValidation
      write FApplyValidation;
  end;

implementation

uses
  DSharp.Core.Reflection,
  TypInfo,
  StrUtils,
  DSharp.Core.Validations;

function Singularize(const AName: string): string;
begin
  if EndsText('ies', AName) then
    Result := LeftStr(AName, Length(AName) - 3) + 'y'
  else if EndsText('s', AName) then
    Result := LeftStr(AName, Length(AName) - 1)
  else
    Result := AName;
end;

function DerivePotentialSelectionNames(const AName: string): TArray<string>;
var
  LSingular: string;
begin
  LSingular := Singularize(AName);
  Result := TArray<string>.Create('Active' + LSingular, 'Selected' + LSingular,
    'Current' + LSingular);
end;

{ ConventionManager }

class function ConventionManager.AddElementConvention<T>(APropertyName,
  AEventName: string): TElementConvention;
begin
  Result := TElementConvention.Create(APropertyName, AEventName);
  FConventions.AddOrSetValue(T, Result);
end;

class procedure ConventionManager.ConfigureSelectedItem(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent;
  ASelectedItemPropertyName: string);
var
  LBindingGroup: TBindingGroup;
  LProperty: TRttiProperty;
  LPotentialName: string;
begin
  LBindingGroup := FindBindingGroup(AViewElement);

  for LPotentialName in DerivePotentialSelectionNames(AViewElement.Name) do
  begin
    LProperty := AViewModel.GetProperty(LPotentialName);
    if Assigned(LProperty) then
    begin
      LBindingGroup.AddBinding(AViewModel, LPotentialName, AViewElement,
        ASelectedItemPropertyName);
      { TODO -o##jwp -cEnhance : Add logging of binding }
    end;
  end;
end;

class constructor ConventionManager.Create;
begin
  FConventions := TObjectDictionary<TClass, TElementConvention>.Create
    ([doOwnsValues]);

  ApplyValidation :=
      procedure(Binding: TBinding; ViewModel: TObject; PropertyName: string)
    begin
      // Reference
      // http://stackoverflow.com/questions/4261138/caliburn-micro-is-it-possible-to-validate-on-exceptions-with-convention-based

      // TODO: New code from CM - this line is disabled for testing purposes
      // if TType.IsAssignable(ViewModel.ClassInfo, TypeInfo(IDataErrorInfo)) then
      begin
        Binding.ValidatesOnDataErrors := True;
      end;
    end;
end;

class destructor ConventionManager.Destroy;
begin
  FConventions.Free();
end;

class function ConventionManager.GetElementConvention(AElementType: TClass)
  : TElementConvention;
begin
  if not FConventions.TryGetValue(AElementType, Result) and
    (AElementType.ClassParent <> nil) then
  begin
    Result := GetElementConvention(AElementType.ClassParent);
  end;
end;

class function ConventionManager.SetBinding(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent; ABindingType: TBindingType;
  AConvention: TElementConvention): Boolean;
var
  LTargetPropertyName: string;
begin
  case ABindingType of
    btProperty:
      LTargetPropertyName := AConvention.PropertyName;
    btEvent:
      LTargetPropertyName := AConvention.EventName;
  end;

  Result := SetBinding(AViewModel, APropertyName, AViewElement,
    LTargetPropertyName);
end;

class function ConventionManager.SetBinding(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent;
  ATargetPropertyName: string): Boolean;
var
  LBindingGroup: TBindingGroup;
  LBinding: TBinding;
begin
  LBindingGroup := FindBindingGroup(AViewElement);
  if not Assigned(LBindingGroup) then
    LBindingGroup := TBindingGroup.Create(AViewElement.Owner);

  // Initialize all `LBinding` properties as parameters here so that `AddBinding` can do the logging
  LBinding := LBindingGroup.AddBinding(AViewModel, APropertyName, AViewElement,
    ATargetPropertyName);

  { TODO -o##jwp -cEnhance : Add logging of validation }

  ApplyValidation(LBinding, AViewModel, APropertyName);

  Result := True;
end;

end.
