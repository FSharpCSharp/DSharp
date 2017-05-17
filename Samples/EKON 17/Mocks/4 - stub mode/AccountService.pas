unit AccountService;

interface

uses
  Account,
  AccountServiceIntf,
  CurrencyServiceIntf,
  LoggerIntf;

type
  TAccountService = class(TInterfacedObject, IAccountService)
  private
    FCurrencyService: ICurrencyService;
    FLogger: ILogger;
  public
    constructor Create(ACurrencyService: ICurrencyService; ALogger: ILogger);

    procedure TransferFunds(ASource, ATarget: TAccount; AAmount: Double);
  end;

implementation

{ TAccountService }

constructor TAccountService.Create(ACurrencyService: ICurrencyService; ALogger: ILogger);
begin
  FCurrencyService := ACurrencyService;
  FLogger := ALogger;
end;

procedure TAccountService.TransferFunds(ASource, ATarget: TAccount;
  AAmount: Double);
var
  LConversionRate: Double;
  LConvertedAmount: Currency;
begin
  ASource.Withdraw(AAmount);
  FLogger.LogMessage('withdrawn amount from source');
  try
    FLogger.LogMessage('getting exchange rate');
    LConversionRate := FCurrencyService.GetConversionRate(ASource.Currency, ATarget.Currency);
    LConvertedAmount := AAmount * LConversionRate;
    ATarget.Deposit(LConvertedAmount);
    FLogger.LogMessage('deposited amount on target');
  except
    on E: ECurrencyServiceException do
    begin
      FLogger.LogMessage('error depositing amount back on source');
      ASource.Deposit(AAmount);
    end;
  end;
end;

end.
