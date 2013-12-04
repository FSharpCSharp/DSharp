unit DSharp.Validation.DataAnnotations;

interface

uses
  SysUtils,
  TypInfo,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Core.Validations,
  DSharp.Core.Reflection;

type
  {$SCOPEDENUMS ON}
  ///	<summary>
  ///	  Represents an enumeration of the data types associated with data fields
  ///	  and parameters.
  ///	</summary>
  ///	<remarks>
  ///	  This enumeration is used to specify the type of data to associate with
  ///	  a data column or a parameter. You use the DataTypeAttribute class to
  ///	  specify the data type you want to associate with the data field or
  ///	  parameter. You select the data type from this enumeration. The
  ///	  DataTypeAttribute attribute lets you mark fields by using a type that
  ///	  is more specific than the database intrinsic types. For example, a
  ///	  string data field that contains e-mail addresses can be attributed with
  ///	  the EmailAddress type. This information can be accessed by the field
  ///	  templates and modify how the data field is processed.
  ///	</remarks>
  TDataType = (
    /// <summary>
    /// Represents a custom data type.
    /// </summary>
    Custom,

    /// <summary>
    /// Represents an instant in time, expressed as a date and time of day.
    /// </summary>
    DateTime,

    /// <summary>
    /// Represents a date value.
    /// </summary>
    Date,

    /// <summary>
    /// Represents a time value.
    /// </summary>
    Time,

    /// <summary>
    /// Represents a continuous time during which an object exists.
    /// </summary>
    Duration,

    /// <summary>
    /// Represents a phone number value.
    /// </summary>
    PhoneNumber,

    /// <summary>
    /// Represents a currency value.
    /// </summary>
    Currency,

    /// <summary>
    /// Represents text that is displayed.
    /// </summary>
    Text,

    /// <summary>
    /// Represents an HTML file.
    /// </summary>
    Html,

    /// <summary>
    /// Represents multi-line text.
    /// </summary>
    MultilineText,

    /// <summary>
    /// Represents an e-mail address.
    /// </summary>
    EmailAddress,

    /// <summary>
    /// Represent a password value.
    /// </summary>
    Password,

    /// <summary>
    /// Represents a URL value.
    /// </summary>
    Url,

    /// <summary>
    /// Represents a URL to an image.
    /// </summary>
    ImageUrl,

    /// <summary>
    /// Represents a credit card number.
    /// </summary>
    CreditCard,

    /// <summary>
    /// Represents a postal code.
    /// </summary>
    PostalCode,

    /// <summary>
    /// Represents file upload data type.
    /// </summary>
    Upload);
  {$SCOPEDENUMS OFF}

  ///	<summary>
  ///	  Specifies the name of an additional type to associate with a data
  ///	  field.
  ///	</summary>
  ///	<remarks>
  ///	  The DataTypeAttribute attribute enables you to mark fields by using a
  ///	  type that is more specific than the database intrinsic type. The type
  ///	  name is selected from the DataType enumeration type. For example, a
  ///	  string data field that contains e-mail addresses can be specified as
  ///	  the EmailAddress type. This information is then accessed by the field
  ///	  templates to modify how the data field is processed.
  ///	</remarks>

  DataTypeAttribute = class(ValidationAttribute)
  private
    FCustomDataType: string;
    FDataType: TDataType;
  public
    ///	<summary>
    ///	  Initializes a new instance of the DataTypeTypeAttribute class by
    ///	  using the specified type name.
    ///	</summary>
    ///	<param name="DataType">
    ///	  The name of the type to associate with the data field.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    ///	<remarks>
    ///	  The name is one of the values that are defined by the
    ///	  System.ComponentModel.DataAnnotations.DataType enumeration.
    ///	</remarks>
    constructor Create(DataType: TDataType;
      const ErrorMessage: string = ''); overload;

    ///	<summary>
    ///	  Initializes a new instance of the DataTypeTypeAttribute class by
    ///	  using the specified field template name.
    ///	</summary>
    ///	<param name="CustomDataType">
    ///	  The name of the custom field template to associate with the data
    ///	  field.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(CustomDataType: string;
      const ErrorMessage: string = ''); overload;

    ///	<summary>
    ///	  Gets the name of custom field template that is associated with the
    ///	  data field.
    ///	</summary>
    ///	<value>
    ///	  The name of the custom field template that is associated with the
    ///	  data field.
    ///	</value>
    property CustomDataType: string read FCustomDataType;

    ///	<summary>
    ///	  Gets the type that is associated with the data field.
    ///	</summary>
    ///	<value>
    ///	  One of the System.ComponentModel.DataAnnotations.DataType values.
    ///	</value>
    property DataType: TDataType read FDataType;
  end;

  ///	<summary>
  ///	  Specifies that a field value must be a valid e-mail address.
  ///	</summary>

  EmailAddressAttribute = class sealed(DataTypeAttribute)
  public
    ///	<summary>
    ///	  Initializes a new instance of the EmailAddressAttribute class.
    ///	</summary>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(const ErrorMessage: string = '');

    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;
  end;

  ///	<summary>
  ///	  Specifies that a field value must match the specified regular
  ///	  expression.
  ///	</summary>

  RegularExpressionAttribute = class sealed(ValidationAttribute)
  private
    FPattern: string;
  public
    ///	<summary>
    ///	  Initializes a new instance of the RegularExpressionAttribute class.
    ///	</summary>
    ///	<param name="Pattern">
    ///	  The regular expression that is used to validate the data field value.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(const Pattern: string; const ErrorMessage: string = '');
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;

    ///	<summary>
    ///	  Gets the regular expression pattern.
    ///	</summary>
    ///	<value>
    ///	  The pattern to match.
    ///	</value>
    property Pattern: string read FPattern;
  end;

  ///	<summary>
  ///	  Specifies the maximum length of characters that are allowed in a data
  ///	  field.
  ///	</summary>

  StringLengthAttribute = class sealed(ValidationAttribute)
  private
    FMaximumLength: Integer;
  public
    ///	<summary>
    ///	  Initializes a new instance of the StringLengthAttribute class by
    ///	  using a specified maximum length.
    ///	</summary>
    ///	<param name="MaximumLength">
    ///	  The maximum length of a string.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(MaximumLength: Integer; const ErrorMessage: string = '');
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;

    ///	<summary>
    ///	  Gets or sets the maximum length of a string.
    ///	</summary>
    ///	<remarks>
    ///	  The maximum length a string.
    ///	</remarks>
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
  end;

  ///	<summary>
  ///	  Specifies the maximum length of array or string data allowed in a
  ///	  property.
  ///	</summary>

  MaxLengthAttribute = class sealed(ValidationAttribute)
  private
    FLength: Integer;
  public
    ///	<summary>
    ///	  Initializes a new instance of the MaxLengthAttribute class based on
    ///	  the length parameter.
    ///	</summary>
    ///	<param name="Length">
    ///	  The maximum allowable length of array or string data.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(Length: Integer; const ErrorMessage: string = '');
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;

    ///	<summary>
    ///	  Gets the maximum allowable length of the array or string data.
    ///	</summary>
    ///	<value>
    ///	  The maximum allowable length of the array or string data.
    ///	</value>
    property Length: Integer read FLength;
  end;

  ///	<summary>
  ///	  Specifies the minimum length of array or string data allowed in a
  ///	  property.
  ///	</summary>

  MinLengthAttribute = class sealed(ValidationAttribute)
  private
    FLength: Integer;
  public
    ///	<summary>
    ///	  Initializes a new instance of the MinLengthAttribute class based on
    ///	  the length parameter.
    ///	</summary>
    ///	<param name="Length">
    ///	  The minimum allowable length of array or string data.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(Length: Integer; const ErrorMessage: string = '');
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;

    ///	<summary>
    ///	  Gets or sets the minimum allowable length of the array or string
    ///	  data.
    ///	</summary>
    ///	<value>
    ///	  The minimum allowable length of the array or string data.
    ///	</value>
    property Length: Integer read FLength write FLength;
  end;

  ///	<summary>
  ///	  Provides URL validation.
  ///	</summary>

  UrlAttribute = class sealed(DataTypeAttribute)
  public
    ///	<summary>
    ///	  Initializes a new instance of the UrlAttribute class.
    ///	</summary>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    constructor Create(const ErrorMessage: string = '');
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;
  end;

  ///	<summary>
  ///	  Specifies a custom validation method that is used to validate a
  ///	  property or class instance.
  ///	</summary>

  CustomValidationAttribute = class sealed(ValidationAttribute)
  private
    FValidatorType: PTypeInfo;
    FMethod: string;
  public
    ///	<summary>
    ///	  Initializes a new instance of the CustomValidationAttribute class.
    ///	</summary>
    ///	<param name="ValidatorType">
    ///	  The type that contains the method that performs custom validation.
    ///	</param>
    ///	<param name="Method">
    ///	  The method that performs custom validation.
    ///	</param>
    ///	<param name="ErrorMessage">
    ///	  The error message that is associated with the validation control.
    ///	</param>
    ///	<remarks>
    ///	  This method specifies a custom class and a related method to call at
    ///	  run time in order to execute custom validation logic.
    ///	</remarks>
    constructor Create(ValidatorType: PTypeInfo; const Method: string;
      const ErrorMessage: string = '');
    function IsValid(const Value: TValue;
      const ValidationContext: TValidationContext): IValidationResult; override;

    ///	<summary>
    ///	  Gets the type that performs custom validation.
    ///	</summary>
    ///	<remarks>
    ///	  The type that performs custom validation.
    ///	</remarks>
    property ValidatorType: PTypeInfo read FValidatorType;

    ///	<summary>
    ///	  Gets the validation method.
    ///	</summary>
    ///	<value>
    ///	  The name of the validation method.
    ///	</value>
    property Method: string read FMethod;
  end;

implementation

uses
  StrUtils,
  DSharp.Core.RegularExpressions,
  Rtti,
  Generics.Collections;

resourcestring
  SEmailAddressAttribute_ValidationError =
    'The field %s is not a valid e-mail address.';
  SRegularExpressionAttribute_ValidationError =
    'The field %s must match the regular expression ''%s''.';
  SStringLengthAttribute_InvalidMaxLength =
    'The maximum length must be a nonnegative integer.';
  SStringLengthAttribute_ValidationError =
    'The field %s must be a string with a maximum length of %d.';
  SMaxLengthAttribute_InvalidLength =
    'The maximum length must be a nonnegative integer.';
  SMaxLengthAttribute_ValidationError =
    'The field %s must be a string with a maximum length of %d.';
  SMinLengthAttribute_InvalidLength =
    'The minimum length must be a nonnegative integer.';
  SMinLengthAttribute_ValidationError =
    'The field %s must be a string with a minimum length of %d.';
  SUrlAttribute_ValidationError = 'The field %s is not a valid URL address.';
  SCustomValidationAttribute_ValidationError = '%s is not valid.';
  SCustomValidationAttribute_Method_Required =
    'The CustomValidationAttribute.Method was not specified.';
  SCustomValidationAttribute_ValidatorType_Required =
    'The CustomValidationAttribute.ValidatorType was not specified.';
  SCustomValidationAttribute_Method_Not_Found =
    'The CustomValidationAttribute method ''%s'' does not exist in type ''%s'' or is not public and static.';
  { EmailAddressAttribute }

constructor EmailAddressAttribute.Create(const ErrorMessage: string = '');
begin
  inherited Create(TDataType.EmailAddress, IfThen(ErrorMessage <> EmptyStr,
    ErrorMessage, SEmailAddressAttribute_ValidationError));
end;

function EmailAddressAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
const
  // https://raw.github.com/mono/aspnetwebstack/master/src/Microsoft.Web.Mvc/EmailAddressAttribute.cs
  CRegex: string =
    '^((([a-z]|\d|[!#\$%&''\*\+\-\/=\?\^_`{\|}~]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])+(\.([a-z]|\d|[!#\$%&''\*\+\-\/=\?\^_`{\|}~]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF'
    + '}\x{FDF0}-\x{FFEF}])+)*)|((\x22)((((\x20|\x09)*(\x0d\x0a))?(\x20|\x09)+)?(([\x01-\x08\x0b\x0c\x0e-\x1f\x7f]|\x21|[\x23-\x5b]|[\x5d-\x7e]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FD'
    + 'F0}-\x{FFEF}])|(\\([\x01-\x09\x0b\x0c\x0d-\x7f]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}]))))*(((\x20|\x09)*(\x0d\x0a))?(\x20|\x09)+)?(\x22)))@((([a-z]|\d|[\x{00A0}-'
    + '\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(([a-z]|\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{F'
    + 'FEF}])*([a-z]|\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])))\.)+(([a-z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(([a-z]|[\x{00A0}-\x{D7FF}\x{F900}-\x'
    + '{FDCF}\x{FDF0}-\x{FFEF}])([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])*([a-z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])))\.?$';

begin
  if Value.IsString and (not TRegex.IsMatch(Value.AsString, CRegex)) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName]));
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

{ RegularExpressionAttribute }

constructor RegularExpressionAttribute.Create(const Pattern: string;
  const ErrorMessage: string);
begin
  inherited Create(IfThen(ErrorMessage <> EmptyStr, ErrorMessage,
    SRegularExpressionAttribute_ValidationError));
  if Pattern = EmptyStr then
    raise EArgumentNilException.Create('Pattern');
  FPattern := Pattern;
end;

function RegularExpressionAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  if Value.IsString and (not TRegex.IsMatch(Value.AsString, Pattern)) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName, Pattern]));
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

{ StringLengthAttribute }

constructor StringLengthAttribute.Create(MaximumLength: Integer;
  const ErrorMessage: string);
begin
  inherited Create(IfThen(ErrorMessage <> EmptyStr, ErrorMessage,
    SStringLengthAttribute_ValidationError));
  if MaximumLength < 0 then
    raise EArgumentOutOfRangeException.Create
      (SStringLengthAttribute_InvalidMaxLength);
  FMaximumLength := MaximumLength;
end;

function StringLengthAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  if Value.IsString and (Length(Value.AsString) > MaximumLength) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName, MaximumLength]));
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

constructor MaxLengthAttribute.Create(Length: Integer;
  const ErrorMessage: string = '');
begin
  inherited Create(IfThen(ErrorMessage <> EmptyStr, ErrorMessage,
    SMaxLengthAttribute_ValidationError));
  if Length < 0 then
    raise EArgumentOutOfRangeException.Create
      (SMaxLengthAttribute_InvalidLength);
  FLength := Length;
end;

function MaxLengthAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  if Value.IsString and (System.Length(Value.AsString) > Length) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName, Length]));
  end
  else if Value.IsArray and (Value.GetArrayLength > Length) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName, Length]))
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

constructor MinLengthAttribute.Create(Length: Integer;
  const ErrorMessage: string = '');
begin
  inherited Create(IfThen(ErrorMessage <> EmptyStr, ErrorMessage,
    SMinLengthAttribute_ValidationError));
  if Length < 0 then
    raise EArgumentOutOfRangeException.Create
      (SMinLengthAttribute_InvalidLength);
  FLength := Length;
end;

function MinLengthAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
begin
  if Value.IsString and (System.Length(Value.AsString) < Length) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName, Length]));
  end
  else if Value.IsArray and (Value.GetArrayLength < Length) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName, Length]))
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

{ UrlAttribute }

constructor UrlAttribute.Create(const ErrorMessage: string = '');
begin
  inherited Create(TDataType.Url, IfThen(ErrorMessage <> EmptyStr, ErrorMessage,
    SUrlAttribute_ValidationError));
end;

function UrlAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
const
  // https://raw.github.com/mono/aspnetwebstack/master/src/Microsoft.Web.Mvc/UrlAttribute.cs
  CRegex: string =
    '^(https?|ftp):\/\/(((([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\da-f]{2})|[!\$&''\(\)\*\+,;=]|:)*@)?(((\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.(\d'
    + '|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.(\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5]))|((([a-z]|\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(('
    + '[a-z]|\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])*([a-z]|\d|[\x{00A0}-\x{D7FF}\x{F900}-\x{'
    + 'FDCF}\x{FDF0}-\x{FFEF}])))\.)+(([a-z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(([a-z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])([a-z]|\d|-|\.|_|~|[\x'
    + '{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])*([a-z]|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])))\.?)(:\d*)?)(\/((([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}'
    + '-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\da-f]{2})|[!\$&''\(\)\*\+,;=]|:|@)+(\/(([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\da-f]{2})|[!\$&''\(\)\*\+,;'
    + '=]|:|@)*)*)?)?(\?((([a-z]|\d|-|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\da-f]{2})|[!\$&''\(\)\*\+,;=]|:|@)|[\x{E000}-\x{F8FF}]|\/|\?)*)?(\#((([a-z]|\d|-'
    + '|\.|_|~|[\x{00A0}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFEF}])|(%[\da-f]{2})|[!\$&''\(\)\*\+,;=]|:|@)|\/|\?)*)?$';
begin
  if Value.IsString and (not TRegex.IsMatch(Value.AsString, CRegex)) then
  begin
    Result := TValidationResult.Create(False,
      Format(ErrorMessage, [ValidationContext.MemberName]));
  end
  else
  begin
    Result := TValidationResult.ValidResult;
  end;
end;

{ DataTypeAttribute }

constructor DataTypeAttribute.Create(DataType: TDataType;
  const ErrorMessage: string);
begin
  inherited Create(ErrorMessage);
  FDataType := DataType;
end;

constructor DataTypeAttribute.Create(CustomDataType: string;
  const ErrorMessage: string);
begin
  Create(TDataType.Custom, ErrorMessage);
  FCustomDataType := CustomDataType;
end;

{ CustomValidationAttribute }

constructor CustomValidationAttribute.Create(ValidatorType: PTypeInfo;
  const Method: string; const ErrorMessage: string = '');
begin
  inherited Create(IfThen(ErrorMessage <> EmptyStr, ErrorMessage,
    SCustomValidationAttribute_ValidationError));

  if not Assigned(ValidatorType) then
    raise EArgumentNilException.Create
      (SCustomValidationAttribute_ValidatorType_Required);
  if Method = EmptyStr then
    raise EArgumentNilException.Create
      (SCustomValidationAttribute_Method_Required);

  FValidatorType := ValidatorType;
  FMethod := Method;
end;

function CustomValidationAttribute.IsValid(const Value: TValue;
  const ValidationContext: TValidationContext): IValidationResult;
var
  LMethod: TRttiMethod;
  LType: TRttiType;
  LValue: TValue;
begin
  LType := GetRttiType(FValidatorType);

  LMethod := LType.GetMethod(Method);
  if not Assigned(LMethod) then
    raise EInvalidOpException.CreateFmt
      (SCustomValidationAttribute_Method_Not_Found, [Method, LType.ToString]);

  LValue := LMethod.Invoke(ValidationContext.ObjectInstance,
    [TValue.From<TValue>(ValidationContext.ObjectInstance),
    TValue.From<TValidationContext>(ValidationContext)]);

  Result := LValue.AsType<IValidationResult>;
end;

end.
