unit DSharp.Validation.DataAnnotationValidationRule;

interface

uses
  Rtti,
  SysUtils,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Validation,
  DSharp.Validation.ValidationRule;

type
  TDataAnnotationValidationRule = class(TValidationRule, IAsyncValidationRule,
    IValidationRule)
  private
    FAttribute: ValidationAttribute;
    FInstance: TObject;
    FProp: TRttiProperty;
  protected
    function GetSupportsSyncValidation: boolean; override;
  public
    constructor Create(Target: IValidationTarget;
      Attribute: ValidationAttribute; Prop: TRttiProperty; Instance: TObject);
    function Evaluate: TRuleResult; override;
    property Attribute: ValidationAttribute read FAttribute;
    property Instance: TObject read FInstance;
    property Prop: TRttiProperty read FProp;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Core.Validations;

{ TDataAnnotationValidationRule }

constructor TDataAnnotationValidationRule.Create(Target: IValidationTarget;
  Attribute: ValidationAttribute; Prop: TRttiProperty; Instance: TObject);
begin
  inherited Create(Target, nil, nil);
  FAttribute := Attribute;
  FProp := Prop;
  FInstance := Instance;
end;

function TDataAnnotationValidationRule.Evaluate: TRuleResult;
var
  LValidationContext: TValidationContext;
  LValidationResult: DSharp.Core.Validations.IValidationResult;
begin
  LValidationContext := TValidationContext.Create(Instance);
  LValidationContext.MemberName := Prop.Name;
  LValidationResult := Attribute.IsValid(FProp.GetValue(Instance),
    LValidationContext);
  Result := TRuleResult.Create(LValidationResult.IsValid,
    [LValidationResult.ErrorContent]);
end;

function TDataAnnotationValidationRule.GetSupportsSyncValidation: boolean;
begin
  Result := True;
end;

end.
