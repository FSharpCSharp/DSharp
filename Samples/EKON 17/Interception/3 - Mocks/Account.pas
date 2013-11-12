unit Account;

interface

type
  TAccount = class
  private
    FCurrency: string;
    FNumber: string;
    FBalance: Currency;
  public
    constructor Create(ANumber, Acurrency: string);

    procedure Withdraw(AAmount: Currency);
    procedure Deposit(AAmount: Currency);

    property Number: string read FNumber write FNumber;
    property Currency: string read FCurrency write FCurrency;
    property Balance: Currency read FBalance;
  end;

implementation

{ TAccount }

constructor TAccount.Create(ANumber, ACurrency: string);
begin
  FNumber := ANumber;
  FCurrency := ACurrency;
end;

procedure TAccount.Deposit(AAmount: Currency);
begin
  FBalance := FBalance + AAmount;
end;

procedure TAccount.Withdraw(AAmount: Currency);
begin
  FBalance := FBalance - AAmount;
end;

end.
