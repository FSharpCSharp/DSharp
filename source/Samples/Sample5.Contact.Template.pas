unit Sample5.Contact.Template;

interface

uses
  System.Data.Templates;

type
  TContactTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetTemplateDataClass: TClass; override;
  end;

implementation

uses
  Sample5.Contact;

{ TContactTemplate }

function TContactTemplate.GetTemplateDataClass: TClass;
begin
  Result := TContact;
end;

function TContactTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
var
  LItem: TContact;
begin
  LItem := Item as TContact;
  case ColumnIndex of
    0: Result := LItem.Lastname + ', ' + LItem.Firstname;
  end;
end;

end.
