unit DSharp.Validation.GenericValidationTarget;

interface

uses
  Rtti,
  Spring,
  SysUtils,
  DSharp.Validation;

type
  TGenericValidationTarget = class(TInterfacedObject, IValidationTarget,
    IEquatable<TGenericValidationTarget>)
  private
    FTargetId: TValue;
  public
    constructor Create(const TargetId: TValue);
    function Equals(Other: TGenericValidationTarget): Boolean;
      reintroduce; overload;
    function Equals(Obj: TObject): Boolean; overload; override;
    function GetHashCode: Integer; override;
    function IsMatch(const Target: TValue): Boolean;
    function UnwrapTargets: TArray<TValue>;
    property TargetId: TValue read FTargetId write FTargetId;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ TGenericValidationTarget }

constructor TGenericValidationTarget.Create(const TargetId: TValue);
begin
  Guard.CheckTrue(not TargetId.IsEmpty, 'TargetId');

  FTargetId := TargetId;
end;

function TGenericValidationTarget.Equals
  (Other: TGenericValidationTarget): Boolean;
begin
  if not Assigned(Other) then
  begin
    Exit(False);
  end;

  if Self = Other then
  begin
    Exit(True);
  end;

  Result := SameValue(Other.TargetId, TargetId);
end;

function TGenericValidationTarget.Equals(Obj: TObject): Boolean;
begin
  if not Assigned(Obj) then
  begin
    Exit(False);
  end;

  if Self = Obj then
  begin
    Exit(True);
  end;

  if not(Obj is TGenericValidationTarget) then
  begin
    Exit(False);
  end;

  Result := Equals(TGenericValidationTarget(Obj));
end;

function TGenericValidationTarget.GetHashCode: Integer;
begin
  if FTargetId.IsEmpty then
  begin
    Result := 0;
  end
  else
  begin
    Result := FTargetId.ToObject.GetHashCode;
  end;
end;

function TGenericValidationTarget.IsMatch(const Target: TValue): Boolean;
begin
  Result := SameValue(Target, TargetId);
end;

function TGenericValidationTarget.UnwrapTargets: TArray<TValue>;
begin
  Result := TArray<TValue>.Create(FTargetId);
end;

end.
