unit DSharp.Validation.RuleResult;

interface

uses
  SysUtils;

type
  ///	<summary>
  ///	  Represents the outcome of a validation rule when executed.
  ///	</summary>
  TRuleResult = record
  private
    FErrors: TArray<string>;
    FIsValid: Boolean;
    function GetErrors: TArray<string>;
    function GetIsValid: Boolean;
  public
    constructor Create(IsValid: Boolean;
      const Errors: array of string); overload;

    ///	<summary>
    ///	  Adds an error to the result.
    ///	</summary>
    ///	<param name="Error">
    ///	  The error message to add.
    ///	</param>
    procedure AddError(const Error: string);

    ///	<summary>
    ///	  Asserts the specified condition and if <c>false</c> then creates and
    ///	  invalid result with the given <paramref name="errorMessage" />. If
    ///	  <c>true</c>, returns a valid result.
    ///	</summary>
    ///	<param name="Condition">
    ///	  The assertion.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message in case if the <paramref name="condition" /> is not
    ///	  <c>true</c>.
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="TRuleResult" /> that represents the result
    ///	  of the assertion.
    ///	</returns>
    class function Assert(Condition: Boolean; const ErrorMessage: string)
      : TRuleResult; static;

    ///	<summary>
    ///	  Creates an "Invalid" result with the given error
    ///	  <paramref name="Error" />.
    ///	</summary>
    ///	<param name="Error">
    ///	  The error text that describes why this rule is invalid.
    ///	</param>
    ///	<returns>
    ///	  An instance of <see cref="TRuleResult" /> that represents an invalid
    ///	  result.
    ///	</returns>
    class function Invalid(const Error: string): TRuleResult; static;

    ///	<summary>
    ///	  Creates a "Valid" result.
    ///	</summary>
    ///	<returns>
    ///	  An instance of <see cref="TRuleResult" /> that represents a valid
    ///	  outcome of the rule.
    ///	</returns>
    class function Valid: TRuleResult; static;

    ///	<summary>
    ///	  Determines whether the specified <see cref="TRuleResult" /> instances
    ///	  are equal.
    ///	</summary>
    class operator Equal(const Left, Right: TRuleResult): Boolean;

    ///	<summary>
    ///	  Determines whether the specified <see cref="TRuleResult" /> instances
    ///	  are not equal.
    ///	</summary>
    class operator NotEqual(const Left, Right: TRuleResult): Boolean;

    ///	<summary>
    ///	  Gets the error messages in case if the target is invalid according to
    ///	  this validation rule.
    ///	</summary>
    property Errors: TArray<string> read GetErrors;

    ///	<summary>
    ///	  Gets a value indicating whether the validation rule passed (valid).
    ///	</summary>
    property IsValid: Boolean read GetIsValid;
  end;

implementation

uses
  Spring,
  StrUtils;

constructor TRuleResult.Create(IsValid: Boolean; const Errors: array of string);
var
  LError: string;
begin
  for LError in Errors do
  begin
    if Length(LError) > 0 then
    begin
      AddError(LError);
    end;
  end;
  FIsValid := IsValid;
end;

procedure TRuleResult.AddError(const Error: string);
begin
  Guard.CheckTrue(Length(Error) > 0, 'Error');

  SetLength(FErrors, Length(FErrors) + 1);
  FErrors[Length(FErrors) - 1] := Error;
  FIsValid := False;
end;

class function TRuleResult.Assert(Condition: Boolean;
  const ErrorMessage: string): TRuleResult;
begin
  if not Condition then
  begin
    Result := Invalid(ErrorMessage);
  end
  else
  begin
    Result := Valid;
  end;
end;

function TRuleResult.GetErrors: TArray<string>;
begin
  Result := FErrors;
end;

function TRuleResult.GetIsValid: Boolean;
begin
  Result := FIsValid;
end;

class function TRuleResult.Invalid(const Error: string): TRuleResult;
begin
  Guard.CheckTrue(Length(Error) > 0, 'Error');

  Result := TRuleResult.Create(False, [Error]);
end;

class function TRuleResult.Valid: TRuleResult;
begin
  Result := TRuleResult.Create(True, []);
end;

class operator TRuleResult.Equal(const Left, Right: TRuleResult): Boolean;
var
  LItem: string;
begin
  if Length(Left.Errors) <> Length(Right.Errors) then
    Exit(False);

  for LItem in Left.Errors do
  begin
    if IndexStr(LItem, Right.Errors) = -1 then
    begin
      Exit(False);
    end;
  end;

  Result := Left.IsValid and Right.IsValid;
end;

class operator TRuleResult.NotEqual(const Left, Right: TRuleResult): Boolean;
begin
  Result := not(Left = Right);
end;

end.
