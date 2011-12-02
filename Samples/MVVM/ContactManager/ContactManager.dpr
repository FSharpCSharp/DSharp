program ContactManager;

uses
  ContactDetails.View in 'ContactDetails.View.pas' {ContactDetailsView: TFrame},
  ContactDetails.ViewModel in 'ContactDetails.ViewModel.pas',
  ContactManager.DemoDataProvider in 'ContactManager.DemoDataProvider.pas',
  ContactManager.Interfaces in 'ContactManager.Interfaces.pas',
  ContactsOverview.View in 'ContactsOverview.View.pas' {ContactsOverviewView: TFrame},
  ContactsOverview.ViewModel in 'ContactsOverview.ViewModel.pas',
  // uncomment one or add your own logging to see the AOP do its work (XE and higher only)
//  DSharp.Logging.CodeSite,
//  DSharp.Logging.SmartInspect,
  DSharp.PresentationModel.VCLApplication,
  Forms;

{$R *.res}

begin
  Application.Initialize();
  Application.Start<IContactsOverviewViewModel>;
  ReportMemoryLeaksOnShutdown := True;
end.
