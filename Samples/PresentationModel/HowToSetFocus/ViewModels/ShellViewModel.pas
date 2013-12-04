unit ShellViewModel;

interface

uses
  Interfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive;

type
  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  TShellViewModel = class(TConductorCollectionOneActive<IScreen>,
    IShellViewModel)
  private
    FFirstName: string;
    { Private declarations }
    FIsFirstNameFocused: Boolean;
    FIsLastNameFocused: Boolean;
    FLastName: string;
    procedure SetFirstName(const Value: string);
    procedure SetIsFirstNameFocused(const Value: Boolean);
    procedure SetIsLastNameFocused(const Value: Boolean);
    procedure SetLastName(const Value: string);
  public
    { Public declarations }
    constructor Create; override;
    procedure ActionFocusFirstName;
    procedure ActionFocusLastName;

    ///	<summary>
    ///	  First name
    ///	</summary>
    property FirstName: string read FFirstName write SetFirstName;

    ///	<summary>
    ///	  Set this property to True to focus First Name edit
    ///	</summary>
    property IsFirstNameFocused: Boolean read FIsFirstNameFocused
      write SetIsFirstNameFocused;

    ///	<summary>
    ///	  Set this property to True to focus Last Name edit
    ///	</summary>
    property IsLastNameFocused: Boolean read FIsLastNameFocused
      write SetIsLastNameFocused;

    ///	<summary>
    ///	  Last name
    ///	</summary>
    property LastName: string read FLastName write SetLastName;
  end;

implementation

{ TShellViewModel }

constructor TShellViewModel.Create;
begin
  inherited;
  // Set form header
  DisplayName := 'Focus extension test';

  // Initialize first name
  FFirstName := 'Captain Haddock';

  // Initialize last name
  FLastName := '';

  // Initialize focus to whatever control should be focused
  FIsLastNameFocused := True;
end;

procedure TShellViewModel.ActionFocusFirstName;
begin
  IsFirstNameFocused := True;
end;

procedure TShellViewModel.ActionFocusLastName;
begin
  IsLastNameFocused := True;
end;

procedure TShellViewModel.SetFirstName(const Value: string);
begin
  if FFirstName <> Value then
  begin
    FFirstName := Value;
    NotifyOfPropertyChange('FirstName');
  end;
end;

procedure TShellViewModel.SetIsFirstNameFocused(const Value: Boolean);
begin
  FIsFirstNameFocused := True;
  NotifyOfPropertyChange('IsFirstNameFocused');
end;

procedure TShellViewModel.SetIsLastNameFocused(const Value: Boolean);
begin
  FIsLastNameFocused := True;
  NotifyOfPropertyChange('IsLastNameFocused');
end;

procedure TShellViewModel.SetLastName(const Value: string);
begin
  if FLastName <> Value then
  begin
    FLastName := Value;
    NotifyOfPropertyChange('LastName');
  end;
end;

initialization

TShellViewModel.ClassName;

end.
