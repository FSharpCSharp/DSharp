unit Sample5.Contact;

interface

uses
  DSharp.Core.PropertyChangedBase;

type
  TContact = class(TPropertyChangedBase)
  private
    FLastname: string;
    FFirstname: string;
    procedure SetFirstname(const Value: string);
    procedure SetLastname(const Value: string);
  public
    constructor Create(AFirstname: string = ''; ALastname: string = '');
    property Firstname: string read FFirstname write SetFirstname;
    property Lastname: string read FLastname write SetLastname;
  end;

implementation

{ TContact }

constructor TContact.Create(AFirstname, ALastname: string);
begin
  FFirstname := AFirstname;
  FLastname := ALastname;
end;

procedure TContact.SetFirstname(const Value: string);
begin
  FFirstname := Value;
  DoPropertyChanged('Firstname');
end;

procedure TContact.SetLastname(const Value: string);
begin
  FLastname := Value;
  DoPropertyChanged('Lastname');
end;

end.
