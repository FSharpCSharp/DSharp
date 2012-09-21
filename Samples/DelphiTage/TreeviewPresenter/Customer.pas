unit Customer;

interface

uses
  Classes,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  Rtti;

type
  TCustomer = class(TPersistent)
  private
    FCustomerId: string;
    FCompanyName: string;
    FContactName: string;
    FContactTitle: string;
    FAddress: string;
    FCity: string;
  published
    property CustomerId: string read FCustomerId write FCustomerId;
    property CompanyName: string read FCompanyName write FCompanyName;
    property ContactName: string read FContactName write FContactName;
    property ContactTitle: string read FContactTitle write FContactTitle;
    property Address: string read FAddress write FAddress;
    property City: string read FCity write FCity;
  end;

  TCustomers = class(TObjectList<TCustomer>);

  TCustomerTemplate = class(TDataTemplate)
  public
    function GetValue(const Item: TObject; const ColumnIndex: Integer): TValue; override;
    function GetTemplateDataClass: TClass; override;
  end;

implementation

{ TCustomerTemplate }

function TCustomerTemplate.GetTemplateDataClass: TClass;
begin
  Result := TCustomer;
end;

function TCustomerTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
begin
  case ColumnIndex of
    -1, 0: Result := TCustomer(Item).CustomerId;
    1: Result := TCustomer(Item).CompanyName;
    2: Result := TCustomer(Item).ContactTitle + ' ' + TCustomer(Item).ContactName;
  end;
end;

initialization
  TCustomers.ClassName;

end.
