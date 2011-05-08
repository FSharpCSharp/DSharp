unit Sample5.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sample5.Contact, Generics.Collections, Collections.ObservableCollection,
  ExtCtrls, StdCtrls, ComCtrls, System.Bindings.Controls.VCL, System.Bindings;

type
  TMainForm = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    BindingGroup1: TBindingGroup;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FContacts: TList<TObject>;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Contacts: TList<TObject> read FContacts;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Sample5.Contact.Template;

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FContacts.Add(TContact.Create);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  FContacts.Remove(ListBox1.CurrentItem);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  FContacts := TObservableCollection<TObject>.Create(True);
  FContacts.Add(TContact.Create('John', 'Doe'));
  FContacts.Add(TContact.Create('Jane', 'Doe'));
  FContacts.Add(TContact.Create('Baby', 'Doe'));
  inherited;
  ListBox1.ItemTemplate := TContactTemplate.Create;
end;

destructor TMainForm.Destroy;
begin
  inherited;
  FContacts.Free();
end;

end.
