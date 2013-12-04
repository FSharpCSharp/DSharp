unit DSharp.Validation.ValidationResultExtensions;

interface

uses
  SysUtils,
  DSharp.Validation;

type
  ///	<summary>
  ///	  Contains helper extension methods for working with
  ///	  <see cref="IValidationResult" />.
  ///	</summary>
  TValidationResultExtensions = record
  public
    ///	<summary>
    ///	  Merges <paramref name="FirstResult" /> with given
    ///	  <paramref name="SecondResult" /> and returns a new instance of
    ///	  <see cref="IValidationResult" />that represents the merged result
    ///	  (the result that contains errors from both results without
    ///	  duplicates).
    ///	</summary>
    ///	<param name="FirstResult">
    ///	  The first validation result to merge.
    ///	</param>
    ///	<param name="SecondResult">
    ///	  The second validation result to merge.
    ///	</param>
    ///	<returns>
    ///	  A new instance of <see cref="IValidationResult" /> that represents
    ///	  the merged result (the result that contains errors from both results
    ///	  whithout duplicates).
    ///	</returns>
    class function Combine(FirstResult: IValidationResult;
      SecondResult: IValidationResult): IValidationResult; static;
  end;

implementation

uses
  DSharp.Validation.ValidationResult,
  DSharp.Validation.ValidationError,
  Spring;

{ TValidationResultExtensions }

class function TValidationResultExtensions.Combine(FirstResult,
  SecondResult: IValidationResult): IValidationResult;
var
  LResult: TValidationResult;
  LError: TValidationError;
begin
  Guard.CheckNotNull(SecondResult, 'SecondResult');

  LResult := TValidationResult.Create;

  for LError in FirstResult.ErrorList do
  begin
    LResult.AddError(LError.Target, LError.ErrorText);
  end;

  for LError in SecondResult.ErrorList do
  begin
    if LResult.ErrorList.Contains(LError) then
    begin
      Continue;
    end;

    LResult.AddError(LError.Target, LError.ErrorText);
  end;

  Result := LResult;
end;

end.
