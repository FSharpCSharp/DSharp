program ContactManager;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  ContactDetails.View
    in 'ContactDetails.View.pas' {ContactDetailsView: TFrame} ,
  ContactDetails.ViewModel in 'ContactDetails.ViewModel.pas',
  ContactManager.DemoDataProvider in 'ContactManager.DemoDataProvider.pas',
  ContactManager.Interfaces in 'ContactManager.Interfaces.pas',
  ContactsOverview.View
    in 'ContactsOverview.View.pas' {ContactsOverviewView: TFrame} ,
  ContactsOverview.ViewModel in 'ContactsOverview.ViewModel.pas',
  Forms;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize();
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
  {$ENDIF CodeSite}
  Application.Start<IContactsOverviewViewModel>();

end.
