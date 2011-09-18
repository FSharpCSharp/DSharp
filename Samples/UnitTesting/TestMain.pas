unit TestMain;

interface

uses
  AccountServiceBase,
  CurrencyServiceBase,
  DSharp.Testing.Mock,
  TestFramework;

type
  TCurrencyServiceTest = class(TTestCase)
  private
    FMockCurrencyService: Mock<TCurrencyServiceBase>;
    FAccountService: TAccountServiceBase;
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
  DSharp.Testing.Verify;

procedure TCurrencyServiceTest.SetUp;
begin
  FMockCurrencyService := Mock<TCurrencyServiceBase>.Create();
  FAccountService := TAccountService.Create(FMockCurrencyService);
end;

procedure TCurrencyServiceTest.TearDown;
begin
  FAccountService.Free();
  FMockCurrencyService.Free();
end;

procedure TCurrencyServiceTest.TransferFunds_UsesCurrencyService;
var
  LAmericanAccount: TAccount;
  LGermanAccount: TAccount;
begin
  LAmericanAccount := TAccount.Create('12345', 'USD');
  LGermanAccount := TAccount.Create('54321', 'EUR');
  LGermanAccount.Deposit(100);

  FMockCurrencyService.WillReturn<Double>(1.38).Once.WhenCalling.GetConversionRate('EUR', 'USD');
  try
    FAccountService.TransferFunds(LGermanAccount, LAmericanAccount, 100);

    Verify.That(LGermanAccount.Balance, ShouldBe.EqualTo<Double>(0));
    Verify.That(LAmericanAccount.Balance, ShouldBe.EqualTo<Double>(138));

    FMockCurrencyService.Verify();
  finally
    LAmericanAccount.Free();
    LGermanAccount.Free();
  end;
end;

initialization
//  ReportMemoryLeaksOnShutdown := True;  // memleak in Delphi XE (see QC 98671)
  RegisterTest(TCurrencyServiceTest.Suite);

end.
