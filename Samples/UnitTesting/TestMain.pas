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
    procedure ShouldUseCurrencyServiceToDetermineConversionRateBetweenAccounts;
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

procedure TCurrencyServiceTest.ShouldUseCurrencyServiceToDetermineConversionRateBetweenAccounts;
var
  LCanadianAccount: TAccount;
  LBritishAccount: TAccount;
begin
  LCanadianAccount := TAccount.Create('12345', 'CAD');
  LBritishAccount := TAccount.Create('54321', 'GBP');
  LBritishAccount.Deposit(100);

  FMockCurrencyService.WillReturn<Double>(2.2).Once.WhenCalling.GetConversionRate('GBP', 'CAD');
  try
    FAccountService.TransferFunds(LBritishAccount, LCanadianAccount, 100);

    Verify.That(LBritishAccount.Balance, ShouldBe.EqualTo<Double>(0));
    Verify.That(LCanadianAccount.Balance, ShouldBe.EqualTo<Double>(220));

    FMockCurrencyService.Verify();
  finally
    LCanadianAccount.Free();
    LBritishAccount.Free();
  end;
end;

initialization
//  ReportMemoryLeaksOnShutdown := True;  // memleak in Delphi XE (see QC 98671)
  RegisterTest(TCurrencyServiceTest.Suite);

end.
