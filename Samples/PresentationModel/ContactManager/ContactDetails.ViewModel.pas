unit ContactDetails.ViewModel;

interface

uses
  SysUtils,
  Contact,
  ContactManager.Interfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ViewModelBase;

type
  TContactDetailsViewModel = class(TEditableViewModel<TContact>,
    IContactDetailsViewModel)
  private
    [Import]
    FWindowManager: IWindowManager;
    function GetContact: TContact;
    procedure SetContact(const Value: TContact);
  protected
    function GetError: string; override;
    function GetItem(const AName: string): string; override;
    property WindowManager: IWindowManager read FWindowManager;
  public
    procedure CanClose(Callback: TProc<Boolean>); override;
    function GetDisplayName: string; override;
    property Contact: TContact read GetContact write SetContact;
  end;

implementation

uses
  DSharp.Core.RegularExpressions,
  StrUtils,
  DSharp.Validation;

function IsValidEmail(const s: string): Boolean;
const
  CRegex: string = '^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}' +
    '\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\' +
    '.)+))([a-zA-Z]{2,5}|[0-9]{1,3})(\]?)$';
begin
  Result := TRegex.IsMatch(s, CRegex);
end;

{ TContactDetailsViewModel }

procedure TContactDetailsViewModel.CanClose(Callback: TProc<Boolean>);
var
  LCanClose: Boolean;
  LValidationResult: IValidationResult;
begin
  LCanClose := True;
  if ModalResult = mrOK then
  begin
    LValidationResult := Validator.ValidateAll;
    if not LValidationResult.IsValid then
    begin
      WindowManager.MessageDlg(LValidationResult.ToString, mtError, [mbOK], 0);
      LCanClose := False;
    end
    else
    begin
      if not IsValidEmail(Contact.Email) then
      begin
        LCanClose := WindowManager.MessageDlg
          ('Email is not valid. Do you want to continue?', mtWarning,
          [mbYes, mbNo], 0) = mrYes;
      end;
    end;
  end;
  Callback(LCanClose);
end;

function TContactDetailsViewModel.GetContact: TContact;
begin
  Result := Subject;
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
  CPropertyNames: array [0 .. 1] of string = ('Contact.FirstName',
    'Contact.LastName');
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
  WithSubject(Value);
end;

initialization

TContactDetailsViewModel.ClassName;

end.
