unit TestMain;

interface

uses
  AccountServiceIntf,
  CurrencyServiceIntf,
  TestFramework;

type
  TCurrencyServiceTest = class(TTestCase)
  private
    FMockCurrencyService: ICurrencyService;
    FAccountService: IAccountService;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TransferFunds_UsesCurrencyService;
  end;

implementation

uses
  Account,
  AccountService,
  CurrencyServiceMock;

procedure TCurrencyServiceTest.SetUp;
begin
  FMockCurrencyService := TCurrencyServiceMock.Create;
  FAccountService := TAccountService.Create(FMockCurrencyService);
end;

procedure TCurrencyServiceTest.TearDown;
begin
  FAccountService := nil;
  FMockCurrencyService := nil;
end;

procedure TCurrencyServiceTest.TransferFunds_UsesCurrencyService;
var
  LAmericanAccount: TAccount;
  LGermanAccount: TAccount;
begin
  LAmericanAccount := TAccount.Create('12345', 'USD');
  LGermanAccount := TAccount.Create('54321', 'EUR');
  LGermanAccount.Deposit(100);

  try
    FAccountService.TransferFunds(LGermanAccount, LAmericanAccount, 100);

    CheckEquals(0, LGermanAccount.Balance, 'german account has wrong balance');
    CheckEquals(138, LAmericanAccount.Balance, 'american account has wrong balance');
  finally
    LAmericanAccount.Free();
    LGermanAccount.Free();
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  RegisterTest(TCurrencyServiceTest.Suite);

end.
