unit DSharp.Validation.ValidationResult;

interface

uses
  Rtti,
  Spring,
  DSharp.Core.Validations,
  DSharp.Collections,
  DSharp.Validation,
  DSharp.Validation.ValidationError;

type
  ///	<summary>
  ///	  Encapsulates result of a validation. Contains a boolean
  ///	  <see cref="IsValid" /> and a collection of errors
  ///	  <see cref="ErrorList" />.
  ///	</summary>
  TValidationResult = class(TInterfacedObject, IValidationResult)
  private
    FErrorList: IList<TValidationError>;
    function GetError(const Target: TValue): string;
    function GetErrorList: IList<TValidationError>;
    function GetIsValid: Boolean;
  public
    constructor Create; overload;
    constructor Create(Target: TValue; Errors: TArray<string>); overload;
    procedure AddError(const Target: TValue; Error: string); overload;
    function ToString: string; reintroduce; overload;
    function ToString(Formatter: IValidationResultFormatter): string;
      reintroduce; overload;
    class function Valid: TValidationResult; static;
    property ErrorList: IList<TValidationError> read GetErrorList;
    property IsValid: Boolean read GetIsValid;
  end;

implementation

uses
  DSharp.Validation.NumberedListValidationResultFormatter,
  DSharp.Core.Reflection;

constructor TValidationResult.Create;
begin
  FErrorList := TList<TValidationError>.Create;
end;

constructor TValidationResult.Create(Target: TValue; Errors: TArray<string>);
var
  LError: string;
begin
  Create;

  for LError in Errors do
  begin
    ErrorList.Add(TValidationError.Create(LError, Target));
  end;
end;

procedure TValidationResult.AddError(const Target: TValue; Error: string);
begin
  ErrorList.Add(TValidationError.Create(Error, Target));
end;

function TValidationResult.GetError(const Target: TValue): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to ErrorList.Count - 1 do
  begin
    if SameValue(ErrorList[i].Target, Target) then
    begin
      Exit(ErrorList[i].ErrorText);
    end;
  end;
end;

function TValidationResult.GetErrorList: IList<TValidationError>;
begin
  Result := FErrorList;
end;

function TValidationResult.GetIsValid: Boolean;
begin
  Result := ErrorList.Count = 0;
end;

function TValidationResult.ToString: string;
var
  LFormatter: IValidationResultFormatter;
begin
  LFormatter := TNumberedListValidationResultFormatter.Create;

  Result := ToString(LFormatter);
end;

function TValidationResult.ToString(Formatter
  : IValidationResultFormatter): string;
begin
  Guard.CheckNotNull(Formatter, 'Formatter');

  Result := Formatter.Format(Self);
end;

class function TValidationResult.Valid: TValidationResult;
begin
  Result := TValidationResult.Create;
end;

end.
