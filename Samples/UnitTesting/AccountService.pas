unit AccountService;

interface

uses
  AccountServiceBase, Account, CurrencyServiceBase;

type
  TAccountService = class(TAccountServiceBase)
  private
    FCurrencyService: TCurrencyServiceBase;
  public
    constructor Create(ACurrencyService: TCurrencyServiceBase);

    procedure TransferFunds(ASource, ATarget: TAccount; AAmount: Currency); override;
  end;

implementation

{ TAccountService }

constructor TAccountService.Create(ACurrencyService: TCurrencyServiceBase);
begin
  FCurrencyService := ACurrencyService;
end;

procedure TAccountService.TransferFunds(ASource, ATarget: TAccount;
  AAmount: Currency);
var
  LConversionRate: Double;
  LConvertedAmount: Currency;
begin
  ASource.Withdraw(AAmount);
  LConversionRate := FCurrencyService.GetConversionRate(ASource.Currency, ATarget.Currency);
  LConvertedAmount := AAmount * LConversionRate;
  ATarget.Deposit(LConvertedAmount);
end;

end.
