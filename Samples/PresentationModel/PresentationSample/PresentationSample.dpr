program PresentationSample;

uses
  DSharp.PresentationModel.VCLApplication,
  Forms,
  zIdentitiesView
    in 'Module\Identities\zIdentitiesView.pas' {IdentitiesView: TFrame} ,
  zIdentitiesViewModel in 'Module\Identities\zIdentitiesViewModel.pas',
  zShellView in 'Module\Shell\zShellView.pas' {ShellView: TScreenForm} ,
  zShellViewModel in 'Module\Shell\zShellViewModel.pas',
  zAccountViewAuthorizations
    in 'Module\Account\Context\zAccountViewAuthorizations.pas' {AccountViewAuthorizations: TFrame} ,
  zAccountViewDetails
    in 'Module\Account\Context\zAccountViewDetails.pas' {AccountViewDetails: TFrame} ,
  zAccountViewGroups
    in 'Module\Account\Context\zAccountViewGroups.pas' {AccountViewGroups: TFrame} ,
  zAccountView in 'Module\Account\zAccountView.pas' {AccountView: TFrame} ,
  zAccountViewModel in 'Module\Account\zAccountViewModel.pas',
  zDocumentsView
    in 'Module\Documents\zDocumentsView.pas' {DocumentsView: TFrame} ,
  zDocumentsViewModel in 'Module\Documents\zDocumentsViewModel.pas',
  Themes,
  Interfaces in 'Framework\Interfaces.pas',
  AppBootstrapper in 'Framework\AppBootstrapper.pas',
  DSharp.Logging,
  zMainView in 'Module\Main\zMainView.pas' {MainView: TFrame} ,
  zMainViewModel in 'Module\Main\zMainViewModel.pas',
  zHeaderView in 'Module\Header\zHeaderView.pas' {HeaderView: TFrame} ,
  zHeaderViewModel in 'Module\Header\zHeaderViewModel.pas',
  zSomeView in 'Module\Some\zSomeView.pas' {SomeView: TFrame} ,
  zSomeViewModel in 'Module\Some\zSomeViewModel.pas',
  zAccount in 'Module\Account\zAccount.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Start(ApplicationBootstrapper);

end.
