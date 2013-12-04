unit AgeToRangeConverter;

interface

uses
  SysUtils,
  DSharp.Core.DataConversion;

type
  TAgeToRangeConverter = class(TValueConverter)
  public
    function Convert(const Value: TValue): TValue; override;
    function ConvertBack(const Value: TValue): TValue; override;
  end;

implementation

{ TAgeToRangeConverter }

function TAgeToRangeConverter.Convert(const Value: TValue): TValue;
begin
  if Value.AsInteger < 25 then
    Result := 'Under 25'
  else
    Result := 'Over 25';
end;

function TAgeToRangeConverter.ConvertBack(const Value: TValue): TValue;
begin
  raise ENotImplemented.Create('');
end;

initialization

TAgeToRangeConverter.ClassName;

end.
