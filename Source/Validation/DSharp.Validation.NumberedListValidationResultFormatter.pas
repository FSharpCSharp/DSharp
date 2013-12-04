unit DSharp.Validation.NumberedListValidationResultFormatter;

interface

uses
  DSharp.Validation;

type
  ///	<summary>
  ///	  An implementation of <see cref="IValidationResultFormatter" /> that
  ///	  formats the validation result as a numbered list of errors or an empty
  ///	  string if valid.
  ///	</summary>
  TNumberedListValidationResultFormatter = class(TInterfacedObject,
    IValidationResultFormatter)
  private
    ///	<summary>
    ///	  Converts the specified validation result object to a string.
    ///	</summary>
    ///	<param name="ValidationResult">
    ///	  The validation result to format.
    ///	</param>
    ///	<returns>
    ///	  A string representation of <paramref name="ValidationResult" />
    ///	</returns>
    function Format(ValidationResult: IValidationResult): string;
  end;

implementation

uses
  DSharp.Collections,
  DSharp.Collections.Extensions,
  DSharp.Validation.ValidationError,
  SysUtils;

function TNumberedListValidationResultFormatter.Format(ValidationResult
  : IValidationResult): string;
var
  LErrors: Enumerable<TValidationError>;
  LDistinctErrorMessages: TArray<string>;
  LResult: TStringBuilder;
  i: integer;
begin
  if ValidationResult.IsValid then
  begin
    Exit('');
  end;

  LErrors := ValidationResult.ErrorList;
  LDistinctErrorMessages := LErrors.Select<string>(
    function(Value: TValidationError): string
    begin
      Result := Value.ErrorText;
    end).Distinct.ToArray;

  if Length(LDistinctErrorMessages) = 1 then
  begin
    Exit(LDistinctErrorMessages[0]);
  end;

  LResult := TStringBuilder.Create;
  try
    for i := 1 to Length(LDistinctErrorMessages) do
    begin
      LResult.AppendFormat('%d. %s', [i, LDistinctErrorMessages[i - 1]]);
      LResult.AppendLine;
    end;

    Result := Trim(LResult.ToString);
  finally
    LResult.Free;
  end;
end;

end.
