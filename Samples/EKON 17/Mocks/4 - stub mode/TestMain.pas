unit TestMain;

interface

uses
  AccountServiceIntf,
  CurrencyServiceIntf,
  LoggerIntf,
  DSharp.Testing.Mock,
  TestFramework,
  Account;

type
  TCurrencyServiceTest = class(TTestCase)
  private
    FMockCurrencyService: Mock<ICurrencyService>;
    FStubLogger: Mock<ILogger>;
    FAccountService: IAccountService;
    FAmericanAccount: TAccount;
    FGermanAccount: TAccount;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TransferFunds_UsesCurrencyService;
    procedure TransferFunds_AmountIsNotWithdrawnWhenException;
  end;

implementation

uses
  AccountService,
  SysUtils;

procedure TCurrencyServiceTest.SetUp;
begin
  FMockCurrencyService := Mock<ICurrencyService>.Create;
  FStubLogger := Mock<ILogger>.Create;
  FStubLogger.Mode := Stub;
  FAccountService := TAccountService.Create(FMockCurrencyService, FStubLogger);

  FAmericanAccount := TAccount.Create('12345', 'USD');
  FGermanAccount := TAccount.Create('54321', 'EUR');
end;

procedure TCurrencyServiceTest.TearDown;
begin
  FGermanAccount.Free;
  FAmericanAccount.Free;
  FAccountService := nil;
  FStubLogger := nil;
  FMockCurrencyService := nil;
end;

procedure TCurrencyServiceTest.TransferFunds_AmountIsNotWithdrawnWhenException;
begin
  FGermanAccount.Deposit(100);
  FGermanAccount.Currency := '';
  FMockCurrencyService.Setup.WillRaise(ECurrencyServiceException, 'unknown currency')
    .Any.WhenCallingWithAnyArguments.GetConversionRate('', '');

  FAccountService.TransferFunds(FGermanAccount, FAmericanAccount, 100);

  CheckEquals(100, FGermanAccount.Balance, 'german account has wrong balance');
  CheckEquals(0, FAmericanAccount.Balance, 'american account has wrong balance');
end;

procedure TCurrencyServiceTest.TransferFunds_UsesCurrencyService;
begin
  FGermanAccount.Deposit(100);
  FMockCurrencyService.Setup.WillReturn<Double>(1.38)
    .Once.WhenCalling.GetConversionRate('EUR', 'USD');

  FAccountService.TransferFunds(FGermanAccount, FAmericanAccount, 100);
//  FAccountService.TransferFunds(FGermanAccount, FAmericanAccount, 100);

  CheckEquals(0, FGermanAccount.Balance, 'german account has wrong balance');
  CheckEquals(138, FAmericanAccount.Balance, 'american account has wrong balance');
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  RegisterTest(TCurrencyServiceTest.Suite);

end.
