unit DemoData;

interface

uses
  DSharp.Collections;

function CreateContactList(): IList<TObject>;
procedure FillContactList(const AObjectList: IList<TObject>);

implementation

uses
  Contact,
  DSharp.Collections.ObservableCollection;

function CreateContactList(): IList<TObject>;
begin
  Result := TObservableCollection<TObject>.Create(True);
  FillContactList(Result);
end;

procedure FillContactList(const AObjectList: IList<TObject>);
var
  LContact: TContact;
begin
  LContact := TContact.Create();
  AObjectList.Add(LContact);
  LContact.Id := '1';
  LContact.LastName := 'Administrator';
  LContact.IsUser := True;
  LContact.Email := 'admin@doe.com';

  LContact := TContact.Create();
  AObjectList.Add(LContact);
  LContact.Id := '2';
  LContact.Firstname := 'John';
  LContact.LastName := 'Doe';
  LContact.Email := 'john.doe@doe.com';
  LContact.Title := 'Mr.';

  LContact := TContact.Create();
  AObjectList.Add(LContact);
  LContact.Id := '3';
  LContact.Firstname := 'Jane';
  LContact.LastName := 'Doe';
  LContact.Email := 'jane.doe@doe.com';
  LContact.Title := 'Mrs.';

end;

end.
