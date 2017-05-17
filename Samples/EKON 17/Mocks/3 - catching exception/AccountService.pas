unit AccountService;

interface

uses
  Account,
  AccountServiceIntf,
  CurrencyServiceIntf;

type
  TAccountService = class(TInterfacedObject, IAccountService)
  private
    FCurrencyService: ICurrencyService;
  public
    constructor Create(ACurrencyService: ICurrencyService);

    procedure TransferFunds(ASource, ATarget: TAccount; AAmount: Double);
  end;

implementation

{ TAccountService }

constructor TAccountService.Create(ACurrencyService: ICurrencyService);
begin
  FCurrencyService := ACurrencyService;
end;

procedure TAccountService.TransferFunds(ASource, ATarget: TAccount;
  AAmount: Double);
var
  LConversionRate: Double;
  LConvertedAmount: Currency;
begin
  ASource.Withdraw(AAmount);
  try
    LConversionRate := FCurrencyService.GetConversionRate(ASource.Currency, ATarget.Currency);
    LConvertedAmount := AAmount * LConversionRate;
    ATarget.Deposit(LConvertedAmount);
  except
    on E: ECurrencyServiceException do
      ASource.Deposit(AAmount);
  end;
end;

end.
