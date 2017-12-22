unit ContactManager.DemoDataProvider;

interface

uses
  Classes,
  DSharp.ComponentModel.Composition,
  Spring.Collections;

type
  [PartCreationPolicy(cpShared)]
  TDemoDataProvider = class
  private
    FContacts: IObjectList;
    function GetContacts: IObjectList;
  public
    constructor Create;
    [Export('DemoData.Contacts')]
    property Contacts: IObjectList read GetContacts;
  end;

implementation

uses
  DemoData,
  DSharp.Collections.ObservableCollection;

{ TDemoDataProvider }

constructor TDemoDataProvider.Create;
begin
  FContacts := TObservableCollection<TObject>.Create() as IObjectList;
  FillContactList(FContacts);
end;

function TDemoDataProvider.GetContacts: IObjectList;
begin
  Result := FContacts;
end;

initialization
  TDemoDataProvider.ClassName;

end.
