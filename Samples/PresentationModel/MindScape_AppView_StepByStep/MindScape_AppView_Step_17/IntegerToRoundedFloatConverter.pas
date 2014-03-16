unit IntegerToRoundedFloatConverter;

interface

uses
  SysUtils,
  DSharp.Core.DataConversion;

type
  TIntegerToRoundedFloatConverter = class(TValueConverter)
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

implementation

{ TIntegerToRoundedFloatConverter }

function TIntegerToRoundedFloatConverter.Convert(const Value: TValue): TValue;
begin
  Result := Value.AsExtended; // Convert is 1-on-1
end;

function TIntegerToRoundedFloatConverter.ConvertBack(const Value: TValue): TValue;
begin
  Result := Round(Value.AsExtended); // convert back from Float to Integer: Round to Int64
end;

initialization
  TIntegerToRoundedFloatConverter.ClassName;
end.
