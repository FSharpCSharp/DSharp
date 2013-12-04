unit DSharp.Validation.PropertyValidationTarget;

interface

uses
  Rtti,
  Spring,
  SysUtils,
  Generics.Defaults,
  DSharp.Validation,
  DSharp.Core.Reflection;

type
  TPropertyValidationTarget = class(TInterfacedObject, IValidationTarget,
    IEquatable<TPropertyValidationTarget>)
  private
    FPropertyName: string;
  public
    constructor Create(PropertyName: string);
    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(Other: TPropertyValidationTarget): Boolean;
      reintroduce; overload;
    function GetHashCode: Integer; override;
    function IsMatch(const Target: TValue): Boolean;
    function UnwrapTargets: TArray<TValue>;
    property PropertyName: string read FPropertyName write FPropertyName;
  end;

implementation

{ TPropertyValidationTarget }

constructor TPropertyValidationTarget.Create(PropertyName: string);
begin
  Guard.CheckTrue(Length(PropertyName) > 0, 'PropertyName');

  FPropertyName := PropertyName;
end;

function TPropertyValidationTarget.Equals(Obj: TObject): Boolean;
begin
  if not Assigned(Obj) then
  begin
    Exit(False);
  end;

  if Self = Obj then
  begin
    Exit(True);
  end;

  if not(Obj is TPropertyValidationTarget) then
  begin
    Exit(False);
  end;

  Result := Equals(TPropertyValidationTarget(Obj));
end;

function TPropertyValidationTarget.Equals
  (Other: TPropertyValidationTarget): Boolean;
begin
  if not Assigned(Other) then
  begin
    Exit(False);
  end;

  if Self = Other then
  begin
    Exit(True);
  end;

  Result := Other.PropertyName = PropertyName;
end;

function TPropertyValidationTarget.GetHashCode: Integer;
begin
  if Length(FPropertyName) = 0 then
  begin
    Result := 0;
  end
  else
  begin
    // Result := FPropertyName.GetHashCode;
    Result := BobJenkinsHash(PChar(FPropertyName)^,
      ByteLength(FPropertyName), 0);
  end;
end;

function TPropertyValidationTarget.IsMatch(const Target: TValue): Boolean;
begin
  Result := SameValue(PropertyName, Target);
end;

function TPropertyValidationTarget.UnwrapTargets: TArray<TValue>;
var
  LValue: TValue;
begin
  LValue := PropertyName;
  Result := TArray<TValue>.Create(LValue);
end;

end.
