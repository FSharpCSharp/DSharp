unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSharp.Windows.ColumnDefinitions, DSharp.Windows.CustomPresenter,
  DSharp.Windows.TreeViewPresenter, VirtualTrees, StdCtrls, ExtCtrls, ComCtrls,
  Grids, DSharp.Bindings.VCLControls, DSharp.Bindings;

type
  TForm7 = class(TForm)
    VirtualStringTree1: TVirtualStringTree;
    TreeViewPresenter1: TTreeViewPresenter;
    Panel1: TPanel;
    Edit1: TEdit;
    Panel2: TPanel;
    LabeledEdit1: TLabeledEdit;
    BindingGroup1: TBindingGroup;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    procedure FilterCustomer(Item: TObject; var Accepted: Boolean);
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

uses
  Customer,
  DSharp.Collections,
  DSharp.Core.XmlSerialization,
  DSharp.Core.XmlSerialization.XmlReader,
  DSharp.Core.XmlSerialization.XmlSerializer,
  Masks;

procedure TForm7.Edit1Change(Sender: TObject);
begin
  TreeViewPresenter1.ApplyFilter;
end;

procedure TForm7.FilterCustomer(Item: TObject; var Accepted: Boolean);
begin
  Accepted := Accepted and MatchesMask(TCustomer(Item).CompanyName, '*' + Edit1.Text + '*');
end;

procedure TForm7.FormCreate(Sender: TObject);
var
  list: IList;
  ser: IXmlSerializer;
  reader: IXmlReader;
begin
  // daten aus Xml lesen ...
  ser := TXmlSerializer.Create;
  reader := TXmlReader.Create('..\..\Customers.xml');
  Supports(ser.Deserialize(reader), IList, list);

  // ... und dem Presenter zuweisen
  TreeViewPresenter1.View.ItemsSource := list;

  // Filter hinzufügen
  TreeViewPresenter1.View.Filter.Add(FilterCustomer);

  // eigenes template zuweisen
  TreeViewPresenter1.View.ItemTemplate := TCustomerTemplate.Create;
end;

end.
