unit VirtualTreeviewSample.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSharp.Windows.ColumnDefinitions, ExtCtrls, StdCtrls, ComCtrls,
  Grids, DSharp.Bindings.VCLControls, xmldom, XMLIntf, msxmldom, XMLDoc,
  DSharp.Bindings, DSharp.Windows.CustomPresenter,
  DSharp.Windows.TreeViewPresenter, VirtualTrees, ActnList;

type
  TMainForm = class(TForm)
    ContactsPresenter: TTreeViewPresenter;
    BindingGroup: TBindingGroup;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    AddContact: TButton;
    DeleteContact: TButton;
    Contacts: TVirtualStringTree;
    Lastname: TLabeledEdit;
    Firstname: TLabeledEdit;
    TabSheet2: TTabSheet;
    InventoryPresenter: TTreeViewPresenter;
    Inventory: TVirtualStringTree;
    XMLDocument1: TXMLDocument;
    NodeName: TLabeledEdit;
    NodeValue: TLabeledEdit;
    SaveXml: TButton;
    SaveDialog: TSaveDialog;
    TabSheet3: TTabSheet;
    TreeView1: TTreeView;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    TabSheet4: TTabSheet;
    StringGrid1: TStringGrid;
    Edit2: TEdit;
    ActionList: TActionList;
    ContactAction: TAction;
    procedure FormCreate(Sender: TObject);
    procedure AddContactClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
    function InventoryPresenterColumnDefinitions0GetText(Sender: TObject;
      ColumnDefinition: TColumnDefinition; Item: TObject): string;
    procedure SaveXmlClick(Sender: TObject);
    procedure ContactActionExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DSharp.Collections,
  DSharp.Collections.ObservableCollection,
  DSharp.Collections.XmlDocument,
  DSharp.Core.XNode,
  DSharp.Core.DataTemplates,
  DSharp.Core.Reflection,
  DSharp.Windows.ColumnDefinitions.XmlDataTemplate,
  Sample5.Contact;

type
  TColumnsDataTemplate = class(TDataTemplate)
  private
    FColumns: array of string;
  public
    constructor Create(const AColumns: array of string);

    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; override;
    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string); override;
  end;

function DataTemplate(const AColumns: array of string): IDataTemplate;
begin
  Result := TColumnsDataTemplate.Create(AColumns);
end;

procedure TMainForm.ContactActionExecute(Sender: TObject);
begin
  if ContactsPresenter.View.CurrentItem <> nil then
    ShowMessageFmt('DoubleClick or Enter on %s', [
      TContact(ContactsPresenter.View.CurrentItem).Firstname]);
end;

procedure TMainForm.AddContactClick(Sender: TObject);
var
  LContact: TContact;
begin
  LContact := TContact.Create('FirstName', 'LastName');
  ContactsPresenter.View.ItemsSource.Add(LContact);
  ContactsPresenter.View.CurrentItem := LContact;
end;

procedure TMainForm.DeleteContactClick(Sender: TObject);
begin
  ContactsPresenter.View.ItemsSource.Remove(ContactsPresenter.View.CurrentItem);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  xmlItems: IXNodeList;
begin
  ContactsPresenter.View.ItemsSource := TObservableCollection<TObject>.Create();
  ContactsPresenter.View.ItemsSource.Add(TContact.Create('John', 'Doe'));
  ContactsPresenter.View.ItemsSource.Add(TContact.Create('Jane', 'Doe'));

  xmlItems := TXNodeList.Create();
  xmlItems.AddRange(
    XMLDocument1.SelectElements('//*[@Stock=''out''] | //*[@Number>=8 or @Number=3]'));

  InventoryPresenter.View.ItemsSource := IList<TObject>(xmlItems);
  InventoryPresenter.View.ItemTemplate := TXmlDataTemplate.Create(InventoryPresenter.ColumnDefinitions);

  TreeView1.View.ItemsSource := IList<TObject>(xmlItems);
  TreeView1.View.ItemTemplate := TXmlDataTemplate.Create(nil);

  ComboBox1.View.ItemsSource := ContactsPresenter.View.ItemsSource;
  ComboBox1.View.ItemTemplate := DataTemplate(['Firstname']);
  ComboBox1.View.ItemsSource.Add(TContact.Create('Baby', 'Doe'));

  // connect the contacts list to the stringgrid
  StringGrid1.View.ItemsSource := ContactsPresenter.View.ItemsSource;
  StringGrid1.View.ItemTemplate := DataTemplate(['Lastname', 'Firstname']);

  // set column captions
  StringGrid1.Rows[0].CommaText := ',Lastname,Firstname';

  // connect navigation of VirtualTreeView (presenter) with StringGrid
  TBinding.Create(ContactsPresenter, 'View.CurrentItem', StringGrid1, 'View.CurrentItem');
end;

function TMainForm.InventoryPresenterColumnDefinitions0GetText(Sender: TObject;
  ColumnDefinition: TColumnDefinition; Item: TObject): string;
var
  LNode: TXNode;
  LAttribute: TXNode;
begin
  LNode := TXNode(Item);

  Result := '<' + LNode.Name;

  for LAttribute in LNode.Attributes do
  begin
    Result := Result + ' ' + LAttribute.Name + '="' + LAttribute.Value + '"';
  end;

  Result := Result + '>';

  if LNode.Value <> '' then
  begin
    Result := Result + LNode.Value +  '</' + LNode.Name + '>';
  end;
end;

procedure TMainForm.SaveXmlClick(Sender: TObject);
begin
  if SaveDialog.Execute() then
  begin
    XMLDocument1.SaveToFile(SaveDialog.FileName);
  end;
end;

{ TColumnsDataTemplate }

constructor TColumnsDataTemplate.Create(const AColumns: array of string);
var
  i: Integer;
begin
  SetLength(FColumns, Length(AColumns));
  for i := Low(FColumns) to High(FColumns) do
  begin
    FColumns[i] := AColumns[i];
  end;
end;

function TColumnsDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if ColumnIndex = -1 then
    Result := Item.GetProperty(FColumns[0]).GetValue(Item).ToString
  else
    if ColumnIndex < Length(FColumns) then
      Result := Item.GetProperty(FColumns[ColumnIndex]).GetValue(Item).ToString
    else
      Result := '';
end;

procedure TColumnsDataTemplate.SetText(const Item: TObject;
  const ColumnIndex: Integer; const Value: string);
begin
  if ColumnIndex = -1 then
    Item.GetProperty(FColumns[0]).TrySetValue(Item, Value)
  else
    if ColumnIndex < Length(FColumns) then
      Item.GetProperty(FColumns[ColumnIndex]).TrySetValue(Item, Value);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
