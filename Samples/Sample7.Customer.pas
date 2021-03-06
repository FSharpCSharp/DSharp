unit Sample7.Customer;

interface

type
  TCustomer = class
  private
    FCustomerId: string;
    FCompanyName: string;
  public
    constructor Create(ACustomerId: string = ''; ACompanyName: string = '');
    property CustomerId: string read FCustomerId write FCustomerId;
    property CompanyName: string read FCompanyName write FCompanyName;
  end;

implementation

uses
  SysUtils;

{ TCustomer }

constructor TCustomer.Create(ACustomerId, ACompanyName: string);
begin
  FCustomerId := ACustomerId;
  FCompanyName := ACompanyName;
end;

end.
