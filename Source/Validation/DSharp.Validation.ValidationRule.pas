unit DSharp.Validation.ValidationRule;

interface

uses
  Spring,
  SysUtils,
  DSharp.Validation;

type
  TValidationRule = class(TInterfacedObject, IAsyncValidationRule)
  private
    FTarget: IValidationTarget;
    FValidationDelegate: TFunc<TRuleResult>;
    FAsyncValidateAction: TAsyncRuleValidateAction;
    function GetTarget: IValidationTarget;
  protected
    function GetSupportsSyncValidation: Boolean; virtual;
  public
    constructor Create(Target: IValidationTarget;
      ValidateDelegate: TFunc<TRuleResult>;
      AsyncValidateAction: TAsyncRuleValidateAction);
    function Evaluate: TRuleResult; virtual;
    procedure EvaluateAsync(Completed: TProc<TRuleResult>);
    property SupportsSyncValidation: Boolean read GetSupportsSyncValidation;
    property Target: IValidationTarget read GetTarget;
    property ValidationDelegate: TFunc<TRuleResult> read FValidationDelegate;
  end;

implementation

uses
  DSharp.PresentationModel.INPC;

constructor TValidationRule.Create(Target: IValidationTarget;
  ValidateDelegate: TFunc<TRuleResult>;
  AsyncValidateAction: TAsyncRuleValidateAction);
begin
  Guard.CheckNotNull(Target, 'Target');

  FTarget := Target;
  FValidationDelegate := ValidateDelegate;
  FAsyncValidateAction := AsyncValidateAction;

  if not Assigned(FAsyncValidateAction) then
  begin
    FAsyncValidateAction := procedure(ResultCallback: TProc<TRuleResult>)
      begin
        ResultCallback(ValidateDelegate());
      end;
  end;
end;

function TValidationRule.Evaluate: TRuleResult;
begin
  if not SupportsSyncValidation then
  begin
    raise ENotSupportedException.Create
      ('Synchronous validation is not supported by this rule. Method EvaluateAsync must be called instead.');
  end;

  Result := ValidationDelegate;
end;

procedure TValidationRule.EvaluateAsync(Completed: TProc<TRuleResult>);
begin
  Guard.CheckTrue(Assigned(Completed), 'Completed');

  if Assigned(FAsyncValidateAction) then
  begin
    FAsyncValidateAction(Completed);
  end
  else
  begin
    Execute.OnBackgroundThread(
      procedure
      var
        LResult: TRuleResult;
      begin
        LResult := Evaluate();
        Completed(LResult);
      end);
  end;
end;

function TValidationRule.GetSupportsSyncValidation: Boolean;
begin
  Result := Assigned(FValidationDelegate);
end;

function TValidationRule.GetTarget: IValidationTarget;
begin
  Result := FTarget;
end;

end.
