unit CurrencyServiceIntf;

interface

uses
  SysUtils;

type
  ICurrencyService = interface(IInvokable)
    ['{A931D38A-8E9B-4B52-95D6-2ED6004D3F7D}']
    function GetConversionRate(const AFromCurrency, AToCurrency: string): Double;
  end;

  ECurrencyServiceException = class(Exception);

implementation

end.
