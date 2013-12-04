unit DSharp.Validation.ValidationError;

interface

uses
  Rtti;

type
  ///	<summary>
  ///	  Represents a validation error.
  ///	</summary>
  TValidationError = record
  public
    ///	<summary>
    ///	  Gets the error message.
    ///	</summary>
    ///	<remarks>
    ///	  This is public field for data binding reasons.
    ///	</remarks>
    ErrorText: string;

    ///	<summary>
    ///	  Gets the Target of the error (a property name or any other arbitrary
    ///	  object).
    ///	</summary>
    ///	<remarks>
    ///	  This is public field for data binding reasons.
    ///	</remarks>
    Target: TValue;

    ///	<summary>
    ///	  Creates an instance of <see cref="TValidationError" />.
    ///	</summary>
    constructor Create(const ErrorText: string; const Target: TValue);

    ///	<summary>
    ///	  Determines whether the specified <see cref="TValidationError" />record
    ///	   s are equal.
    ///	</summary>
    class operator Equal(const Left, Right: TValidationError): Boolean;

    ///	<summary>
    ///	  Implicit conversion of TValidationError to string
    ///	</summary>
    class operator Implicit(const Value: TValidationError): string;

    ///	<summary>
    ///	  Determines whether the specified <see cref="TValidationError" />record
    ///	   s are not equal.
    ///	</summary>
    class operator NotEqual(const Left, Right: TValidationError): Boolean;
  end;

implementation

uses
  DSharp.Core.Reflection;

constructor TValidationError.Create(const ErrorText: string;
  const Target: TValue);
begin
  Self.ErrorText := ErrorText;
  Self.Target := Target;
end;

class operator TValidationError.Implicit(const Value: TValidationError): string;
begin
  Result := Value.ErrorText;
end;

class operator TValidationError.Equal(const Left,
  Right: TValidationError): Boolean;
begin
  Result := (Left.ErrorText = Right.ErrorText) and
    SameValue(Left.Target, Right.Target);
end;

class operator TValidationError.NotEqual(const Left,
  Right: TValidationError): Boolean;
begin
  Result := not(Left = Right);
end;

end.
