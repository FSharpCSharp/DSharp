unit VirtualTreeviewSample.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, System.Bindings.Controls.VCL, xmldom,
  XMLIntf, msxmldom, XMLDoc, System.Bindings, System.Windows.TreeViewPresenter,
  VirtualTrees;

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
    procedure FormCreate(Sender: TObject);
    procedure AddContactClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function InventoryPresenterColumnDefinitions0GetText(const Item: TObject;
      const ColumnIndex: Integer): string;
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
  Collections.ObservableCollection,
  Collections.Xml,
  Sample5.Contact,
  System.Windows.ColumnDefinitions.Template.Xml;

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

  InventoryPresenter.ItemsSource := TObservableCollection<TObject>.Create(True);
  InventoryPresenter.ItemsSource.AddRange(TArray<TObject>(
    XMLDocument1.SelectElements('//*[@Stock=''out''] | //*[@Number>=8 or @Number=3]')));

  InventoryPresenter.ItemTemplate := TXmlDataTemplate.Create(InventoryPresenter.ColumnDefinitions);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ContactsPresenter.ItemsSource.Free();
  InventoryPresenter.ItemsSource.Free();
end;

function TMainForm.InventoryPresenterColumnDefinitions0GetText(
  const Item: TObject; const ColumnIndex: Integer): string;
begin
  Result := TXNode(Item).SelectValue;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
