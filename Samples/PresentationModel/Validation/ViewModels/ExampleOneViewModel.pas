unit ExampleOneViewModel;

interface

uses
  SysUtils,
  Interfaces,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Validation.DataAnnotations,
  DSharp.Core.Validations,
  // TODO: Remove this unit when old and new validation are merged
  DSharp.Validation,
  DSharp.PresentationModel,
  DSharp.Core.Nullable,
  UserRegistrationServiceIntf,
  PasswordMeterIntf;

type
  ///	<summary>
  ///	  Implementation of <see cref="IExampleOneViewModel" />
  ///	</summary>
  TExampleOneViewModel = class(TValidatingScreen, IExampleOneViewModel)
  private
    FEmailAddress: string;
    FFirstName: string;
    FInterestSelector: IInterestSelectorViewModel;
    FIsValid: Nullable<Boolean>;
    FLastName: string;
    FMasterCard: string;
    FPassword: string;
    FPasswordConfirmation: string;
    FPasswordMeter: IPasswordMeter;
    FPasswordStrength: TPasswordStrengthResult;
    FUrl: string;
    FUserName: string;
    FUserRegistrationService: IUserRegistrationService;
    FValidationErrorsString: string;
    FWorkingAge: Integer;
    procedure OnSelectedInterestsChanged(Sender: TObject);
    procedure OnValidationResultChanged(Sender: TObject;
      Args: IValidationResultChangedEventArgs);
    procedure SetEmailAddress(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetIsValid(const Value: Nullable<Boolean>);
    procedure SetLastName(const Value: string);
    procedure SetMasterCard(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetPasswordConfirmation(const Value: string);
    procedure SetPasswordStrength(const Value: TPasswordStrengthResult);
    procedure SetUrl(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetValidationErrorsString(const Value: string);
    procedure SetWorkingAge(const Value: Integer);
    procedure UpdateValidationSummary(ValidationResult: IValidationResult);
    // [dcc32 Hint] H2219 Private symbol 'UserRegistrationService' declared but never used
    // property UserRegistrationService: IUserRegistrationService read FUserRegistrationService write FUserRegistrationService;
  public
    constructor Create(UserRegistrationService: IUserRegistrationService;
      InterestSelector: IInterestSelectorViewModel;
      PasswordMeter: IPasswordMeter);
    procedure CanClose(Callback: TProc<Boolean>); override;
    procedure ConfigureValidationRules;
    function MyCustomValidation(const Value: TValue;
      const ValidationContext: TValidationContext)
      : DSharp.Core.Validations.IValidationResult;
    procedure Validate;
    [Required]
    [EmailAddress]
    property EmailAddress: string read FEmailAddress write SetEmailAddress;
    [Required]
    property FirstName: string read FFirstName write SetFirstName;
    property InterestSelector: IInterestSelectorViewModel
      read FInterestSelector;
    property IsValid: Nullable<Boolean> read FIsValid write SetIsValid;
    [Required]
    property LastName: string read FLastName write SetLastName;
    [RegularExpression('^5[1-5][0-9]{14}$')]
    property MasterCard: string read FMasterCard write SetMasterCard;
    [Required]
    [MinLength(6)]
    [MaxLength(20)]
    property Password: string read FPassword write SetPassword;
    [Required]
    property PasswordConfirmation: string read FPasswordConfirmation
      write SetPasswordConfirmation;
    property PasswordStrength: TPasswordStrengthResult read FPasswordStrength
      write SetPasswordStrength;
    [Url]
    property Url: string read FUrl write SetUrl;
    [Required]
    property UserName: string read FUserName write SetUserName;
    property ValidationErrorsString: string read FValidationErrorsString
      write SetValidationErrorsString;
    [Range(18, 65)]
    [CustomValidation(TypeInfo(TExampleOneViewModel), 'MyCustomValidation')]
    property WorkingAge: Integer read FWorkingAge write SetWorkingAge;
  end;

implementation

constructor TExampleOneViewModel.Create(UserRegistrationService
  : IUserRegistrationService; InterestSelector: IInterestSelectorViewModel;
  PasswordMeter: IPasswordMeter);
begin
  inherited Create;

  if not Assigned(UserRegistrationService) then
    raise EArgumentNilException.Create('UserRegistrationService');

  if not Assigned(InterestSelector) then
    raise EArgumentNilException.Create('InterestSelector');

  if not Assigned(PasswordMeter) then
    raise EArgumentNilException.Create('PasswordMeter');

  DisplayName := 'DSharp Validation with Data Annotation Attributes';
  FInterestSelector := InterestSelector;
  FInterestSelector.SelectedInterestsChanged.Add(OnSelectedInterestsChanged);

  FUserRegistrationService := UserRegistrationService;
  FPasswordMeter := PasswordMeter;
  ConfigureValidationRules;
  Validator.ResultChanged.Add(OnValidationResultChanged);
end;

procedure TExampleOneViewModel.CanClose(Callback: TProc<Boolean>);
begin
  Callback(True);
end;

procedure TExampleOneViewModel.ConfigureValidationRules;
begin
  // Validator.AddChildValidatable('InterestSelector'); TODO
end;

function TExampleOneViewModel.MyCustomValidation(const Value: TValue;
  const ValidationContext: TValidationContext)
  : DSharp.Core.Validations.IValidationResult;
begin
  Result := TValidationResult.ValidResult;
end;

procedure TExampleOneViewModel.OnSelectedInterestsChanged(Sender: TObject);
var
  LCurrentState: IValidationResult;
begin
  LCurrentState := Validator.GetResult('InterestSelector');
  if not LCurrentState.IsValid then
  begin
    Validator.ValidateAsync('InterestSelector');
  end;
end;

procedure TExampleOneViewModel.OnValidationResultChanged(Sender: TObject;
  Args: IValidationResultChangedEventArgs);
var
  LValidationResult: IValidationResult;
begin
  if not IsValid.GetValueOrDefault(True) then
  begin
    LValidationResult := Validator.GetResult;
    UpdateValidationSummary(LValidationResult);
  end;
end;

procedure TExampleOneViewModel.SetEmailAddress(const Value: string);
begin
  FEmailAddress := Value;
  NotifyOfPropertyChange('EmailAddress');
  Validator.Validate('EmailAddress');
end;

procedure TExampleOneViewModel.SetFirstName(const Value: string);
begin
  FFirstName := Value;
  NotifyOfPropertyChange('FirstName');
  Validator.Validate('FirstName');
end;

procedure TExampleOneViewModel.SetIsValid(const Value: Nullable<Boolean>);
begin
  FIsValid := Value;
  NotifyOfPropertyChange('IsValid');
end;

procedure TExampleOneViewModel.SetLastName(const Value: string);
begin
  FLastName := Value;
  NotifyOfPropertyChange('LastName');
  Validator.Validate('LastName');
end;

procedure TExampleOneViewModel.SetMasterCard(const Value: string);
begin
  FMasterCard := Value;
  NotifyOfPropertyChange('MasterCard');
  Validator.Validate('MasterCard');
end;

procedure TExampleOneViewModel.SetPassword(const Value: string);
begin
  // Set new password
  FPassword := Value;
  // Notify password changed
  NotifyOfPropertyChange('Password');
  // Validate new password
  Validator.Validate('Password');
  // Calculate new password strength
  PasswordStrength := FPasswordMeter.PasswordStrength(Value);
end;

procedure TExampleOneViewModel.SetPasswordConfirmation(const Value: string);
begin
  FPasswordConfirmation := Value;
  NotifyOfPropertyChange('PasswordConfirmation');
  Validator.Validate('PasswordConfirmation');
end;

procedure TExampleOneViewModel.SetPasswordStrength
  (const Value: TPasswordStrengthResult);
begin
  FPasswordStrength := Value;
  NotifyOfPropertyChange('PasswordStrength');
end;

procedure TExampleOneViewModel.SetUrl(const Value: string);
begin
  FUrl := Value;
  NotifyOfPropertyChange('Url');
  Validator.Validate('Url');
end;

procedure TExampleOneViewModel.SetUserName(const Value: string);
begin
  FUserName := Value;
  NotifyOfPropertyChange('UserName');
  Validator.Validate('UserName');
end;

procedure TExampleOneViewModel.SetValidationErrorsString(const Value: string);
begin
  FValidationErrorsString := Value;
  NotifyOfPropertyChange('ValidationErrorsString');
end;

procedure TExampleOneViewModel.SetWorkingAge(const Value: Integer);
begin
  FWorkingAge := Value;
  NotifyOfPropertyChange('WorkingAge');
  Validator.Validate('WorkingAge');
end;

procedure TExampleOneViewModel.UpdateValidationSummary(ValidationResult
  : IValidationResult);
begin
  IsValid := ValidationResult.IsValid;
  ValidationErrorsString := ValidationResult.ToString;
end;

procedure TExampleOneViewModel.Validate;
var
  LValidationResult: IValidationResult;
begin
  LValidationResult := Validator.ValidateAll;
  UpdateValidationSummary(LValidationResult);
end;

initialization

TExampleOneViewModel.ClassName;

end.
