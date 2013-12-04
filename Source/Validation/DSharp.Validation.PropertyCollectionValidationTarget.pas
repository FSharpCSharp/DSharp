unit DSharp.Validation.PropertyCollectionValidationTarget;

interface

uses
  Rtti,
  Spring,
  SysUtils,
  DSharp.Validation;

type
  TPropertyCollectionValidationTarget = class(TInterfacedObject,
    IValidationTarget)
  private
    FProperties: TArray<string>;
  public
    constructor Create(const Properties: array of string);
    function IsMatch(const Target: TValue): Boolean;
    function UnwrapTargets: TArray<TValue>;
    property Properties: TArray<string> read FProperties;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ TPropertyCollectionValidationTarget }

constructor TPropertyCollectionValidationTarget.Create(const Properties
  : array of string);
var
  i: Integer;
begin
  Guard.CheckTrue(Length(Properties) > 0, 'Properties');

  SetLength(FProperties, Length(Properties));
  for i := Low(Properties) to High(Properties) do
  begin
    FProperties[i] := Properties[i];
  end;
end;

function TPropertyCollectionValidationTarget.IsMatch(const Target
  : TValue): Boolean;
var
  LItem: string;
begin
  Result := False;
  for LItem in FProperties do
  begin
    if SameValue(Target, LItem) then
    begin
      Exit(True);
    end;
  end;
end;

function TPropertyCollectionValidationTarget.UnwrapTargets: TArray<TValue>;
var
  i: Integer;
begin
  SetLength(Result, Length(Properties));
  for i := Low(Properties) to High(Properties) do
  begin
    Result[i] := Properties[i];
  end;
end;

end.
