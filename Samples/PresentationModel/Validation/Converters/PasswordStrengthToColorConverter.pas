unit PasswordStrengthToColorConverter;

interface

uses
  SysUtils,
  DSharp.Core.DataConversion,
  PasswordMeterIntf;

type
  TPasswordStrengthToColorConverter = class(TValueConverter)
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

implementation

uses
  Graphics;

{ TPasswordStrengthToColorConverter }

function TPasswordStrengthToColorConverter.Convert(const Value: TValue): TValue;
var
  LPasswordStrength: TPasswordStrengthResult;
begin
  LPasswordStrength := Value.AsType<TPasswordStrengthResult>;
  case LPasswordStrength.Strength of
    TPasswordStrengthEnum.TooShort:
      begin
        Result := $003303AF;
      end;
    TPasswordStrengthEnum.Bad:
      begin
        Result := $003303AF;
      end;
    TPasswordStrengthEnum.Weak:
      begin
        Result := $0051CFD4;
      end;
    TPasswordStrengthEnum.Good:
      begin
        Result := $0080CFC9;
      end;
    TPasswordStrengthEnum.Strong:
      begin
        Result := $0000B485;
      end;
  end;
end;

function TPasswordStrengthToColorConverter.ConvertBack
  (const Value: TValue): TValue;
begin
  raise ENotImplemented.Create('');
end;

initialization

TPasswordStrengthToColorConverter.ClassName;

end.
