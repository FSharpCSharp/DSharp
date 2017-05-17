unit ContactManager.DemoDataProvider;

interface

uses
  Classes,
  DSharp.Collections,
  DSharp.ComponentModel.Composition;

type
  [PartCreationPolicy(cpShared)]
  TDemoDataProvider = class
  private
    FContacts: IList;
    function GetContacts: IList;
  public
    constructor Create;
    [Export('DemoData.Contacts')]
    property Contacts: IList read GetContacts;
  end;

implementation

uses
  DemoData,
  DSharp.Collections.ObservableCollection;

{ TDemoDataProvider }

constructor TDemoDataProvider.Create;
begin
  FContacts := TObservableCollection<TObject>.Create();
  FillContactList(FContacts);
end;

function TDemoDataProvider.GetContacts: IList;
begin
  Result := FContacts;
end;

initialization
  TDemoDataProvider.ClassName;

end.
