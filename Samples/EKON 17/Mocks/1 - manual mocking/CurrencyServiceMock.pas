unit CurrencyServiceMock;

interface

uses
  CurrencyServiceIntf;

type
  TCurrencyServiceMock = class(TInterfacedObject, ICurrencyService)
    function GetConversionRate(const AFromCurrency, AToCurrency: string): Double;
  end;

implementation

{ TCurrencyServiceMock }

function TCurrencyServiceMock.GetConversionRate(const AFromCurrency,
  AToCurrency: string): Double;
begin
  Result := 1.38;
end;

end.
