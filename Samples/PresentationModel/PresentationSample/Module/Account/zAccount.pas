unit zAccount;

interface

type
  TAccount = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

implementation

end.
