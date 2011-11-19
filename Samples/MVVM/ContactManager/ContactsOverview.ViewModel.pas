unit ContactsOverview.ViewModel;

interface

uses
  Contact,
  ContactManager.Interfaces,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.Core.Lazy,
  DSharp.PresentationModel.ViewModelBase;

type
  TContactsOverviewViewModel = class(TViewModelBase, IContactsOverviewViewModel)
  private
    [Import('DemoData.Contacts')]
    FContacts: IList;
    [Import]
    FContactDetails: Lazy<IContactDetailsViewModel>;
    FSelectedContact: TContact;
    function GetCanDeleteContact: Boolean;
    function GetCanEditContact: Boolean;
    function GetSelectedContact: TContact;
    procedure SetSelectedContact(const Value: TContact);
  public
    procedure AddNewContact;
    procedure EditContact;
    procedure DeleteContact;

    property CanDeleteContact: Boolean read GetCanDeleteContact;
    property CanEditContact: Boolean read GetCanEditContact;
    property ContactDetails: Lazy<IContactDetailsViewModel> read FContactDetails;
    property Contacts: IList read FContacts;
    property SelectedContact: TContact read GetSelectedContact write SetSelectedContact;
  end;

implementation

uses
  DSharp.PresentationModel.WindowManager;

{ TContactManagerViewModel }

procedure TContactsOverviewViewModel.AddNewContact;
var
  LContact: TContact;
begin
  LContact := TContact.Create();
  ContactDetails.Value.Contact := LContact;

  if WindowManager.ShowDialog(ContactDetails) = mrOk then
  begin
    FContacts.Add(LContact);
    SelectedContact := LContact;
  end
  else
  begin
    LContact.Free();
  end;
end;

procedure TContactsOverviewViewModel.DeleteContact;
begin
  if WindowManager.MessageDlg('Do you really want to delete the selected contact?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FContacts.Remove(FSelectedContact);
  end;
end;

procedure TContactsOverviewViewModel.EditContact;
begin
  FContactDetails.Value.Contact := FSelectedContact;

  WindowManager.ShowDialog(FContactDetails);
end;

function TContactsOverviewViewModel.GetCanDeleteContact: Boolean;
begin
  Result := Assigned(FSelectedContact)
    and (FSelectedContact is TContact) and not (FSelectedContact as TContact).IsUser;
end;

function TContactsOverviewViewModel.GetCanEditContact: Boolean;
begin
  Result := Assigned(FSelectedContact);
end;

function TContactsOverviewViewModel.GetSelectedContact: TContact;
begin
  Result := FSelectedContact;
end;

procedure TContactsOverviewViewModel.SetSelectedContact(const Value: TContact);
begin
  FSelectedContact := Value;
  DoPropertyChanged('CanDeleteContact');
  DoPropertyChanged('CanEditContact');
  DoPropertyChanged('SelectedContact');
end;

initialization
  TContactsOverviewViewModel.ClassName;

end.
