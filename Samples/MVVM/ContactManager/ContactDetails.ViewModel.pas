unit ContactDetails.ViewModel;

interface

uses
  Contact,
  ContactManager.Interfaces,
  DSharp.PresentationModel.EditableViewModelBase;

type
  TContactDetailsViewModel = class(TEditableViewModelBase<TContact>, IContactDetailsViewModel)
  private
    function GetContact: TContact;
    procedure SetContact(const Value: TContact);
  protected
    function CanClose: Boolean; override;
    function GetDisplayName: string; override;
    function GetError: string; override;
    function GetItem(const AName: string): string; override;
  public
    property Contact: TContact read GetContact write SetContact;
  end;

implementation

uses
  DSharp.Core.RegularExpressions,
  DSharp.PresentationModel.WindowManager,
  StrUtils;

function IsValidEmail(const s: string): Boolean;
const
  CRegex: string = '^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}' +
    '\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\' +
    '.)+))([a-zA-Z]{2,5}|[0-9]{1,3})(\]?)$';
begin
  Result := TRegex.IsMatch(s, CRegex);
end;

{ TContactDetailsViewModel }

function TContactDetailsViewModel.CanClose: Boolean;
begin
  Result := inherited;

  if ValidationErrors.Count > 0 then
  begin
    WindowManager.MessageDlg(ValidationErrors[0].ErrorContent, mtError, [mbOK], 0);
    Result := False;
  end
  else
  begin
    if not IsValidEmail(Contact.Email) then
    begin
      Result := WindowManager.MessageDlg('Email is not valid. Do you want to continue?', mtWarning, [mbYes, mbNo], 0) = mrYes;
    end;
  end;
end;

function TContactDetailsViewModel.GetContact: TContact;
begin
  Result := FItem;
end;

function TContactDetailsViewModel.GetDisplayName: string;
begin
  Result := 'Contact details';
end;

function TContactDetailsViewModel.GetError: string;
begin
  Result := GetItem('Contact.FirstName');
end;

function TContactDetailsViewModel.GetItem(const AName: string): string;
const
  CPropertyNames: array[0..1] of string = (
    'Contact.FirstName',
    'Contact.LastName'
  );
begin
  case IndexText(AName, CPropertyNames) of
    0, 1:
    begin
      if (Contact.Firstname = '') and (Contact.LastName = '') then
        Result := 'Firstname or Lastname must not be empty';
    end;
  end;
end;

procedure TContactDetailsViewModel.SetContact(const Value: TContact);
begin
  inherited SetItem(Value);
end;

initialization
  TContactDetailsViewModel.ClassName;

end.
