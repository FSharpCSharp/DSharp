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
  FAccountService := TAccountService.Create(FMockCurrencyService);
end;

procedure TCurrencyServiceTest.TearDown;
begin
  FAccountService.Free();
  FMockCurrencyService.Clear();
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
    Verify.That(LAmericanAccount.Balance, ShouldBe.EqualTo<Double>(138), 'this fails in 64-bit (see QC 99028)');

    FMockCurrencyService.Verify();
  finally
    LAmericanAccount.Free();
    LGermanAccount.Free();
  end;
end;

initialization
{$IF COMPILERVERSION > 22} // memleak in Delphi XE - fixed in XE2 (see QC 98671)
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
  RegisterTest(TCurrencyServiceTest.Suite);

end.
