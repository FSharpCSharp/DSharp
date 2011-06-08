unit VirtualTreeviewSample.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, System.Bindings.Controls.VCL,
  System.Bindings, System.Windows.TreeViewPresenter, VirtualTrees;

type
  TMainForm = class(TForm)
    Contacts: TVirtualStringTree;
    ContactsPresenter: TTreeViewPresenter;
    BindingGroup: TBindingGroup;
    Lastname: TLabeledEdit;
    Firstname: TLabeledEdit;
    AddContact: TButton;
    DeleteContact: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AddContactClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ContactsPresenter.ItemsSource.Free();
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
