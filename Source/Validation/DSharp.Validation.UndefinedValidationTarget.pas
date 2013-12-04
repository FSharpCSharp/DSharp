unit DSharp.Validation.UndefinedValidationTarget;

interface

uses
  Rtti,
  DSharp.Validation;

type
  TUndefinedValidationTarget = class(TInterfacedObject, IValidationTarget)
  const
    CUndefinedValidationTarget = 'UndefinedValidationTarget';
  public
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function IsMatch(const Target: TValue): Boolean;
    function UnwrapTargets: TArray<TValue>;
  end;

implementation

function TUndefinedValidationTarget.Equals(Obj: TObject): Boolean;
begin
  if not Assigned(Obj) then
  begin
    Exit(True);
  end;

  if Obj is TUndefinedValidationTarget then
  begin
    Exit(True);
  end;

  Result := False;
end;

function TUndefinedValidationTarget.GetHashCode: Integer;
begin
  Result := 0;
end;

function TUndefinedValidationTarget.IsMatch(const Target: TValue): Boolean;
begin
  Result := Target.IsEmpty;
end;

function TUndefinedValidationTarget.UnwrapTargets: TArray<TValue>;
begin
  Result := TArray<TValue>.Create(CUndefinedValidationTarget);
end;

end.
