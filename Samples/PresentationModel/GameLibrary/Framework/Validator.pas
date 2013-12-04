unit Validator;

interface

uses
  SysUtils,
  Error,
  ValidatorIntf,
  Rtti,
  DSharp.Collections;

type
  TValidator = class(TInterfacedObject, IValidator)
  private
    function GetValidationErrors(Instance: TObject; Prop: TRttiProperty)
      : IEnumerable<TError>;
  public
    function Validate(Instance: TObject): IEnumerable<TError>; overload;
    function Validate(Instance: TObject; PropertyName: string)
      : IEnumerable<TError>; overload;
  end;

implementation

uses
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Bindings.Validations,
  DSharp.Core.Validations,
  DSharp.Core.Reflection;

{ TValidator }

function TValidator.GetValidationErrors(Instance: TObject; Prop: TRttiProperty)
  : IEnumerable<TError>;
var
  LAttribute: ValidationAttribute;
  LValidationContext: TValidationContext;
  LValidationResult: IValidationResult;
  LResult: IList<TError>;
begin
  LResult := TObjectList<TError>.Create(True);

  LValidationContext := TValidationContext.Create(Instance);
  LValidationContext.MemberName := Prop.Name;
  for LAttribute in Prop.GetCustomAttributes<ValidationAttribute> do
  begin
    LValidationResult := LAttribute.IsValid(Prop.GetValue(Instance),
      LValidationContext);
    if not LValidationResult.IsValid then
      LResult.Add(TError.Create(Instance, Prop.Name,
        LValidationResult.ErrorContent));
  end;
  Result := LResult;
end;

function TValidator.Validate(Instance: TObject): IEnumerable<TError>;
var
  LProperty: TRttiProperty;
  LResult: IList<TError>;
  LError: TError;
begin
  LResult := TObjectList<TError>.Create(True);
  for LProperty in Instance.GetProperties do
  begin
    for LError in GetValidationErrors(Instance, LProperty) do
      LResult.Add(LError.Clone);
  end;
  Result := LResult;
end;

function TValidator.Validate(Instance: TObject; PropertyName: string)
  : IEnumerable<TError>;
var
  LProperty: TRttiProperty;
begin
  LProperty := Instance.GetProperty(PropertyName);
  Result := GetValidationErrors(Instance, LProperty);
end;

initialization

TValidator.ClassName;

end.
