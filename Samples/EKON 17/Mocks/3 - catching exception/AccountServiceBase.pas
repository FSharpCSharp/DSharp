unit AccountServiceBase;

interface

uses
  Account;

type
  TAccountServiceBase = class
  public
    procedure TransferFunds(ASource, ATarget: TAccount; AAmount: Currency); virtual; abstract;
  end;

implementation

end.
