unit DSharp.Validation.ValidationResultChangedEventArgs;

interface

uses
  Rtti,
  Spring,
  SysUtils,
  DSharp.Core.EventArgs,
  DSharp.Validation;

type
  TValidationResultChangedEventArgs = class(TEventArgs,
    IValidationResultChangedEventArgs)
  private
    FNewResult: IValidationResult;
    FTarget: TValue;
    function GetNewResult: IValidationResult;
    function GetTarget: TValue;
  public
    constructor Create(Target: TValue; NewResult: IValidationResult);
  end;

implementation

constructor TValidationResultChangedEventArgs.Create(Target: TValue;
  NewResult: IValidationResult);
begin
  Guard.CheckNotNull(NewResult, 'NewResult');

  FTarget := Target;
  FNewResult := NewResult;
end;

function TValidationResultChangedEventArgs.GetNewResult: IValidationResult;
begin
  Result := FNewResult;
end;

function TValidationResultChangedEventArgs.GetTarget: TValue;
begin
  Result := FTarget;
end;

end.
