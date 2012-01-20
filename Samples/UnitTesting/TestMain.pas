unit TestMain;

interface

uses
  AccountServiceIntf,
  CurrencyServiceIntf,
  DSharp.Testing.Mock,
  TestFramework;

type
  TCurrencyServiceTest = class(TTestCase)
  private
    FMockCurrencyService: Mock<ICurrencyService>;
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
  DSharp.Aspects.Logging,
  DSharp.Aspects.Weaver,
  // uncomment one or add your own logging to see the AOP do its work (XE and higher only)
//  DSharp.Logging.CodeSite,
//  DSharp.Logging.SmartInspect,
  DSharp.Testing.Verify;

procedure TCurrencyServiceTest.SetUp;
begin
  FMockCurrencyService := Mock<ICurrencyService>.Create;
  FAccountService := TAccountService.Create(
    AspectWeaver.Proxify(FMockCurrencyService.Instance));
  AspectWeaver.AddAspect(ICurrencyService, TLoggingAspect, '.*');
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

  FMockCurrencyService.WillReturn<Double>(1.38).Exactly(1).WhenCalling.GetConversionRate('EUR', 'USD');

//  FMockCurrencyService.WillExecute(
//    function(const Args: TArray<TValue>): TValue
//    begin
//      Result := 1.38
//    end).Once.WhenCallingWithAnyArguments.GetConversionRate('', '');
  try
    FAccountService.TransferFunds(LGermanAccount, LAmericanAccount, 100);

    Verify.That(LGermanAccount.Balance, ShouldBe.EqualTo<Double>(0), 'german account has wrong balance');
    Verify.That(LAmericanAccount.Balance, ShouldBe.EqualTo<Double>(138), 'american account has wrong balance');

    FMockCurrencyService.Verify();
  finally
    LAmericanAccount.Free();
    LGermanAccount.Free();
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
  RegisterTest(TCurrencyServiceTest.Suite);

end.
