unit Sample5.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sample5.Contact, DSharp.Collections.ObservableCollection,
  Spring.Collections, ExtCtrls, StdCtrls, ComCtrls, Grids,
  DSharp.Bindings.VCLControls, DSharp.Bindings;

type
  TMainForm = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    AddContact: TButton;
    DeleteContact: TButton;
    BindingGroup1: TBindingGroup;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    SelectFirst: TButton;
    MovePrevious: TButton;
    MoveNext: TButton;
    procedure AddContactClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
  private
    { Private declarations }
    FContacts: IList<TObject>;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property Contacts: IList<TObject> read FContacts;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Sample5.Contact.Template;

{ TMainForm }

procedure TMainForm.AddContactClick(Sender: TObject);
var
  LContact: TContact;
begin
  LContact := TContact.Create('FirstName', 'LastName');
  FContacts.Add(LContact);
  ListBox1.View.CurrentItem := LContact;
end;

procedure TMainForm.DeleteContactClick(Sender: TObject);
begin
  FContacts.Remove(ListBox1.View.CurrentItem);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  FContacts := TObservableCollection<TObject>.Create(True);
  FContacts.Add(TContact.Create('John', 'Doe'));
  FContacts.Add(TContact.Create('Jane', 'Doe'));
  FContacts.Add(TContact.Create('Baby', 'Doe'));
  inherited;
  ListBox1.View.ItemTemplate := TContactTemplate.Create;
//  ComboBox1.View.ItemTemplate := TContactTemplate.Create;
end;

end.
