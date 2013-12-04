unit ExampleTwoViewModel;

interface

uses
  SysUtils,
  Interfaces,
  DSharp.Validation,
  DSharp.PresentationModel,
  DSharp.Core.Nullable,
  UserRegistrationServiceIntf,
  PasswordMeterIntf;

type
  ///	<summary>
  ///	  Implementation of <see cref="IExampleTwoViewModel" />
  ///	</summary>
  TExampleTwoViewModel = class(TValidatingScreen, IExampleTwoViewModel)
  private
    FInterestSelector: IInterestSelectorViewModel;
    FEmailAddress: string;
    FFirstName: string;
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
    procedure ConfigureValidationRules;
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
    property UserRegistrationService: IUserRegistrationService
      read FUserRegistrationService write FUserRegistrationService;
  public
    constructor Create(UserRegistrationService: IUserRegistrationService;
      InterestSelector: IInterestSelectorViewModel;
      PasswordMeter: IPasswordMeter);
    procedure CanClose(Callback: TProc<Boolean>); override;
    procedure Validate;
    property EmailAddress: string read FEmailAddress write SetEmailAddress;
    property FirstName: string read FFirstName write SetFirstName;
    property InterestSelector: IInterestSelectorViewModel
      read FInterestSelector;
    property IsValid: Nullable<Boolean> read FIsValid write SetIsValid;
    property LastName: string read FLastName write SetLastName;
    property MasterCard: string read FMasterCard write SetMasterCard;
    property Password: string read FPassword write SetPassword;
    property PasswordConfirmation: string read FPasswordConfirmation
      write SetPasswordConfirmation;
    property PasswordStrength: TPasswordStrengthResult read FPasswordStrength
      write SetPasswordStrength;
    property Url: string read FUrl write SetUrl;
    property UserName: string read FUserName write SetUserName;
    property ValidationErrorsString: string read FValidationErrorsString
      write SetValidationErrorsString;
    property WorkingAge: Integer read FWorkingAge write SetWorkingAge;
  end;

implementation

uses
  Character,
  DSharp.Core.RegularExpressions;

constructor TExampleTwoViewModel.Create(UserRegistrationService
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

  DisplayName := 'DSharp Validation with Rule Engine';
  FInterestSelector := InterestSelector;
  FInterestSelector.SelectedInterestsChanged.Add(OnSelectedInterestsChanged);

  FUserRegistrationService := UserRegistrationService;
  FPasswordMeter := PasswordMeter;

  ConfigureValidationRules();
  Validator.ResultChanged.Add(OnValidationResultChanged);
end;

procedure TExampleTwoViewModel.CanClose(Callback: TProc<Boolean>);
begin
  Callback(True);
end;

procedure TExampleTwoViewModel.ConfigureValidationRules;
begin
  Validator.AddRequiredRule('UserName', 'User Name is required');

  Validator.AddAsyncRule('UserName',
    procedure(OnCompleted: TProc<TRuleResult>)
    var
      LIsAvailable: Boolean;
      LRuleResult: TRuleResult;
    begin
      LIsAvailable := UserRegistrationService.IsUserNameAvailable(UserName);

      LRuleResult := TRuleResult.Assert(LIsAvailable,
        Format('User Name %s is taken. Please choose a different one.',
        [UserName]));

      OnCompleted(LRuleResult);
    end);

  Validator.AddRequiredRule('FirstName', 'First Name is required');

  Validator.AddRequiredRule('LastName', 'Last Name is required');

  Validator.AddRequiredRule('EmailAddress', 'Email is required');

  Validator.AddRule('EmailAddress',
    function: TRuleResult
    const
      RegexPattern
        : string =
        '^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$';
    begin
      Result := TRuleResult.Assert(TRegEx.IsMatch(EmailAddress, RegexPattern),
        'Email must be a valid email address');
    end);

  Validator.AddRequiredRule('Password', 'Password is required');

  Validator.AddRule('Password',
    function: TRuleResult
    begin
      Result := TRuleResult.Assert(Length(Password) >= 6,
        'Password must contain at least 6 characters');
    end);

  Validator.AddRule('Password',
    function: TRuleResult
    var
      LChar: Char;
    begin
      for LChar in Password do
        if
        {$IF CompilerVersion >= 26} // Delphi XE5+ has moved to TCharHelper
          LChar.IsDigit()
        {$ELSE}
          IsDigit(LChar)
        {$IFEND CompilerVersion >= 26} // Delphi XE5+ has moved to TCharHelper
        then
          Exit(TRuleResult.Valid);
      Result := TRuleResult.Invalid('Password must contain at least one digit');
    end);

  Validator.AddRule('PasswordConfirmation',
    function: TRuleResult
    begin
      if (Length(Password) > 0) and (Length(PasswordConfirmation) = 0) then
      begin
        Exit(TRuleResult.Invalid('Please confirm password'));
      end;

      Result := TRuleResult.Valid;
    end);

  Validator.AddRule(['Password', 'PasswordConfirmation'],
    function: TRuleResult
    begin
      if (Length(Password) > 0) and (Length(PasswordConfirmation) > 0) then
      begin
        Exit(TRuleResult.Assert(Password = PasswordConfirmation,
          'Passwords do not match'));
      end;

      Result := TRuleResult.Valid;
    end);

  // Validator.AddChildValidatable('InterestSelector'); TODO
end;

procedure TExampleTwoViewModel.OnSelectedInterestsChanged(Sender: TObject);
var
  LCurrentState: IValidationResult;
begin
  LCurrentState := Validator.GetResult('InterestSelector');
  if not LCurrentState.IsValid then
  begin
    Validator.ValidateAsync('InterestSelector');
  end;
end;

procedure TExampleTwoViewModel.OnValidationResultChanged(Sender: TObject;
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

procedure TExampleTwoViewModel.SetEmailAddress(const Value: string);
begin
  FEmailAddress := Value;
  NotifyOfPropertyChange('EmailAddress');
  Validator.Validate('EmailAddress');
end;

procedure TExampleTwoViewModel.SetFirstName(const Value: string);
begin
  FFirstName := Value;
  NotifyOfPropertyChange('FirstName');
  Validator.Validate('FirstName');
end;

procedure TExampleTwoViewModel.SetIsValid(const Value: Nullable<Boolean>);
begin
  FIsValid := Value;
  NotifyOfPropertyChange('IsValid');
end;

procedure TExampleTwoViewModel.SetLastName(const Value: string);
begin
  FLastName := Value;
  NotifyOfPropertyChange('LastName');
  Validator.Validate('LastName');
end;

procedure TExampleTwoViewModel.SetMasterCard(const Value: string);
begin
  FMasterCard := Value;
  NotifyOfPropertyChange('MasterCard');
  Validator.Validate('MasterCard');
end;

procedure TExampleTwoViewModel.SetPassword(const Value: string);
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

procedure TExampleTwoViewModel.SetPasswordConfirmation(const Value: string);
begin
  FPasswordConfirmation := Value;
  NotifyOfPropertyChange('PasswordConfirmation');
  Validator.Validate('PasswordConfirmation');
end;

procedure TExampleTwoViewModel.SetPasswordStrength
  (const Value: TPasswordStrengthResult);
begin
  FPasswordStrength := Value;
  NotifyOfPropertyChange('PasswordStrength');
end;

procedure TExampleTwoViewModel.SetUrl(const Value: string);
begin
  FUrl := Value;
  NotifyOfPropertyChange('Url');
  Validator.Validate('Url');
end;

procedure TExampleTwoViewModel.SetUserName(const Value: string);
begin
  FUserName := Value;
  NotifyOfPropertyChange('UserName');
  Validator.ValidateAsync('UserName');
end;

procedure TExampleTwoViewModel.SetValidationErrorsString(const Value: string);
begin
  FValidationErrorsString := Value;
  NotifyOfPropertyChange('ValidationErrorsString');
end;

procedure TExampleTwoViewModel.SetWorkingAge(const Value: Integer);
begin
  FWorkingAge := Value;
  NotifyOfPropertyChange('WorkingAge');
  Validator.Validate('WorkingAge');
end;

procedure TExampleTwoViewModel.UpdateValidationSummary(ValidationResult
  : IValidationResult);
begin
  IsValid := ValidationResult.IsValid;
  ValidationErrorsString := ValidationResult.ToString;
end;

procedure TExampleTwoViewModel.Validate;
var
  LValidationResult: IValidationResult;
begin
  // TODO
  // LValidationResult := Validator.ValidateAll;
  // UpdateValidationSummary(LValidationResult);

  LValidationResult := Validator.ValidateAllAsync.Value;
  UpdateValidationSummary(LValidationResult);
end;

initialization

TExampleTwoViewModel.ClassName;

end.
