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
    FContacts: IList<TObject>;
    function GetContacts: IList<TObject>;
  public
    constructor Create;
    [Export('DemoData.Contacts')]
    property Contacts: IList<TObject> read GetContacts;
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

function TDemoDataProvider.GetContacts: IList<TObject>;
begin
  Result := FContacts;
end;

initialization
  TDemoDataProvider.ClassName;

end.
