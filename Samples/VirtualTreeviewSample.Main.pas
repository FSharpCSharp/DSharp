unit VirtualTreeviewSample.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSharp.Windows.ColumnDefinitions, ExtCtrls, StdCtrls, ComCtrls,
  DSharp.Bindings.VCLControls, xmldom, XMLIntf, msxmldom, XMLDoc,
  DSharp.Bindings, DSharp.Windows.TreeViewPresenter, VirtualTrees;

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
    procedure FormCreate(Sender: TObject);
    procedure AddContactClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function InventoryPresenterColumnDefinitions0GetText(Sender: TObject;
      ColumnDefinition: TColumnDefinition; Item: TObject): string;
    procedure SaveXmlClick(Sender: TObject);
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
  DSharp.Collections.ObservableCollection,
  DSharp.Collections.XmlDocument,
  DSharp.Core.XNode,
  DSharp.Windows.ColumnDefinitions.XmlDataTemplate,
  Sample5.Contact;

procedure TMainForm.AddContactClick(Sender: TObject);
var
  LContact: TContact;
begin
  LContact := TContact.Create('FirstName', 'LastName');
  ContactsPresenter.ItemsSource.Add(LContact);
  ContactsPresenter.CurrentItem := LContact;
end;

procedure TMainForm.DeleteContactClick(Sender: TObject);
begin
  ContactsPresenter.ItemsSource.Remove(ContactsPresenter.CurrentItem);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ContactsPresenter.ItemsSource := TObservableCollection<TObject>.Create();
  ContactsPresenter.ItemsSource.Add(TContact.Create('John', 'Doe'));
  ContactsPresenter.ItemsSource.Add(TContact.Create('Jane', 'Doe'));

  InventoryPresenter.ItemsSource := TObservableCollection<TObject>.Create();
  InventoryPresenter.ItemsSource.AddRange(TArray<TObject>(
    XMLDocument1.SelectElements('//*[@Stock=''out''] | //*[@Number>=8 or @Number=3]')));

  InventoryPresenter.ItemTemplate := TXmlDataTemplate.Create(InventoryPresenter.ColumnDefinitions);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ContactsPresenter.ItemsSource.Free();
  InventoryPresenter.ItemsSource.Free();
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
    Result := Result + ' ' + LAttribute.Name + '"' + LAttribute.Value + '"';
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

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
