unit CurrencyServiceBase;

interface

type
  TCurrencyServiceBase = class
  public
    function GetConversionRate(AFromCurrency, AToCurrency: string): Double; virtual; abstract;
  end;

implementation

end.
