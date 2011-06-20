unit Sample1.ValueConverters;

interface

uses
  DSharp.Bindings,
  Rtti;

type
  TIntToStr = class(TInterfacedObject, IValueConverter)
  public
    function Convert(Value: TValue): TValue;
    function ConvertBack(Value: TValue): TValue;
  end;

implementation

uses
  SysUtils;

{ TIntToStr }

function TIntToStr.Convert(Value: TValue): TValue;
begin
  Result := TValue.From<string>(Value.ToString());
end;

function TIntToStr.ConvertBack(Value: TValue): TValue;
begin
  Result := TValue.From<Integer>(StrToInt(Value.ToString));
end;

end.
