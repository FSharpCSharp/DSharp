unit Contact;

interface

uses
  Classes;

type
  TPreferedContactMethod = (
    pcPhone,
    pcEmail
  );

  TContact = class(TPersistent)
  private
    FId: string;
    FFirstName: string;
    FLastName: string;
    FPreferedContact: TPreferedContactMethod;
    FEmail: string;
    FFax: string;
    FPhone: string;
    FTitle: string;
    FIsUser: Boolean;
    FAddress: string;
    FZipCode: string;
    FCity: string;
    FCountry: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Id: string read FId write FId;
    property Firstname: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property Title: string read FTitle write FTitle;
    property Phone: string read FPhone write fPhone;
    property Fax: string read FFax write FFax;
    property Email: string read FEmail write FEmail;
    property PreferedContact: TPreferedContactMethod read FPreferedContact write FPreferedContact;
    property IsUser: Boolean read FIsUser write FIsUser;
    property Address: string read FAddress write FAddress;
    property ZipCode: string read FZipCode write FZipCode;
    property City: string read FCity write FCity;
    property Country: string read FCountry write FCountry;
  end;

implementation

{ TContactData }

procedure TContact.Assign(Source: TPersistent);
var
  LSource: TContact;
begin
  LSource := Source as TContact;
  FId := LSource.Id;
  FFirstName := LSource.FirstName;
  FLastName := LSource.LastName;
  FPreferedContact := LSource.PreferedContact;
  FEmail := LSource.Email;
  FFax := LSource.Fax;
  FPhone := LSource.Phone;
  FTitle := LSource.Title;
  FIsUser := LSource.IsUser;
  FAddress := LSource.Address;
  FZipCode := LSource.ZipCode;
  FCity := LSource.City;
  FCountry := LSource.Country;
end;

end.
