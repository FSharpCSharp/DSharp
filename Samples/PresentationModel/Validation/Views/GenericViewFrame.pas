unit GenericViewFrame;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  Generic view for validation sample
  ///	</summary>
  TGenericView = class(TFrame)
    DisplayName: TLabel;
    Label2: TLabel;
    UserName: TEdit;
    [Binding('Hint',
      '{Binding ElementName=UserName, Path=Validation.Errors[0].ErrorContent}')]
    [Binding('Visible',
      '{Binding ElementName=UserName, Path=Validation.HasError}')]
    UserNameErrorImage: TImage;
    Label3: TLabel;
    FirstName: TEdit;
    [Binding('Hint',
      '{Binding ElementName=FirstName, Path=Validation.Errors[0].ErrorContent}')
      ]
    [Binding('Visible',
      '{Binding ElementName=FirstName, Path=Validation.HasError}')]
    FirstNameErrorImage: TImage;
    Label5: TLabel;
    LastName: TEdit;
    [Binding('Hint',
      '{Binding ElementName=LastName, Path=Validation.Errors[0].ErrorContent}')]
    [Binding('Visible',
      '{Binding ElementName=LastName, Path=Validation.HasError}')]
    LastNameErrorImage: TImage;
    Label7: TLabel;
    EmailAddress: TEdit;
    [Binding('Hint',
      '{Binding ElementName=EmailAddress, Path=Validation.Errors[0].ErrorContent}')
      ]
    [Binding('Visible',
      '{Binding ElementName=EmailAddress, Path=Validation.HasError}')]
    EmailErrorImage: TImage;
    Label9: TLabel;
    Password: TEdit;
    [Binding('Hint',
      '{Binding ElementName=Password, Path=Validation.Errors[0].ErrorContent}')]
    [Binding('Visible',
      '{Binding ElementName=Password, Path=Validation.HasError}')]
    PasswordErrorImage: TImage;
    Label11: TLabel;
    PasswordConfirmation: TEdit;
    [Binding('Hint',
      '{Binding ElementName=PasswordConfirmation, Path=Validation.Errors[0].ErrorContent}')
      ]
    [Binding('Visible',
      '{Binding ElementName=PasswordConfirmation, Path=Validation.HasError}')]
    PasswordConfirmationErrorImage: TImage;
    Label15: TLabel;
    Url: TEdit;
    [Binding('Hint',
      '{Binding ElementName=Url, Path=Validation.Errors[0].ErrorContent}')]
    [Binding('Visible', '{Binding ElementName=Url, Path=Validation.HasError}')]
    UrlErrorImage: TImage;
    Label14: TLabel;
    WorkingAge: TEdit;
    [Binding('Hint',
      '{Binding ElementName=WorkingAge, Path=Validation.Errors[0].ErrorContent}')
      ]
    [Binding('Visible',
      '{Binding ElementName=WorkingAge, Path=Validation.HasError}')]
    WorkingAgeErrorImage: TImage;
    Label12: TLabel;
    MasterCard: TEdit;
    [Binding('Hint',
      '{Binding ElementName=MasterCard, Path=Validation.Errors[0].ErrorContent}')
      ]
    [Binding('Visible',
      '{Binding ElementName=MasterCard, Path=Validation.HasError}')]
    MasterCardErrorImage: TImage;
    [Binding('Caption',
      '{Binding Path=WorkingAge, Converter=TAgeToRangeConverter}')]
    WorkingAgeRange: TLabel;
    ErrorsSummaryContainer: TPanel;
    Label4: TLabel;
    ValidationErrorsString: TMemo;
    Validate: TButton;
    [Binding('Color',
      '{Binding Path=PasswordStrength, Converter=TPasswordStrengthToColorConverter}')
      ]
    [Binding('Hint', '{Binding Path=PasswordStrength.Hints}')]
    PasswordQuality: TEdit;
    Label1: TLabel;
    InterestSelector: TPanel;
    Label6: TLabel;
    Label8: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  ///	<summary>
  ///	  View for IExampleOneViewModel
  ///	</summary>
  TExampleOneView = class(TGenericView);

  ///	<summary>
  ///	  View for IExampleTwoViewModel
  ///	</summary>
  TExampleTwoView = class(TGenericView);

implementation

{$R *.dfm}

initialization

TGenericView.ClassName;
TExampleOneView.ClassName;
TExampleTwoView.ClassName;

end.
