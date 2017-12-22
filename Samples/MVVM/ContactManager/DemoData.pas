unit DemoData;

interface

uses
  Spring.Collections;

function CreateContactList: IObjectList;
procedure FillContactList(const AObjectList: IObjectList);

implementation

uses
  Contact,
  DSharp.Collections.ObservableCollection;

function CreateContactList: IObjectList;
begin
  Result := TObservableCollection<TObject>.Create(True) as IObjectList;
  FillContactList(Result);
end;

procedure FillContactList(const AObjectList: IObjectList);
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
