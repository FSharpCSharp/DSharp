unit AccountServiceIntf;

interface

uses
  Account;

type
  IAccountService = interface(IInvokable)
    ['{CB97C5AB-64F8-413F-8CE3-0C3090D098D6}']
    procedure TransferFunds(ASource, ATarget: TAccount; AAmount: Double);
  end;

implementation

end.
