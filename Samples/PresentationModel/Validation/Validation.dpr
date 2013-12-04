program Validation;

uses
  {$IFNDEF MadExcept}
  {$ENDIF MadExcept}
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  Interfaces in 'Interfaces.pas',
  ShellViewModel in 'ViewModels\ShellViewModel.pas',
  ShellViewForm in 'Views\ShellViewForm.pas' {ShellView} ,
  ExampleOneViewModel in 'ViewModels\ExampleOneViewModel.pas',
  ExampleTwoViewModel in 'ViewModels\ExampleTwoViewModel.pas',
  GenericViewFrame in 'Views\GenericViewFrame.pas' {GenericView: TFrame} ,
  UserRegistrationServiceIntf in 'Services\UserRegistrationServiceIntf.pas',
  UserRegistrationService in 'Services\UserRegistrationService.pas',
  PasswordMeterIntf in 'Services\PasswordMeterIntf.pas',
  PasswordMeter in 'Services\PasswordMeter.pas',
  AgeToRangeConverter in 'Converters\AgeToRangeConverter.pas',
  PasswordStrengthToColorConverter
    in 'Converters\PasswordStrengthToColorConverter.pas',
  InterestItemViewFrame
    in 'Views\InterestItemViewFrame.pas' {InterestItemView: TFrame} ,
  InterestItemViewModel in 'ViewModels\InterestItemViewModel.pas',
  InterestSelectorViewFrame
    in 'Views\InterestSelectorViewFrame.pas' {InterestSelectorView: TFrame} ,
  InterestSelectorViewModel in 'ViewModels\InterestSelectorViewModel.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
  {$ENDIF CodeSite}
  Application.Start<IShellViewModel>();

end.
