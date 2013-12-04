program PresentationSampleFMX;

uses
  DSharp.PresentationModel.FMXApplication,
  FMX.Forms,
  zShellView in 'Module\Shell\zShellView.pas' {ShellView} ,
  Interfaces in 'Framework\Interfaces.pas',
  zIdentitiesView in 'Module\Identities\zIdentitiesView.pas' {IdentitiesView} ,
  zDocumentsView in 'Module\Documents\zDocumentsView.pas' {DocumentsView} ,
  zDocumentsViewModel in 'Module\Documents\zDocumentsViewModel.pas',
  zIdentitiesViewModel in 'Module\Identities\zIdentitiesViewModel.pas',
  zShellViewModel in 'Module\Shell\zShellViewModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Start<IShell>();
  ReportMemoryLeaksOnShutdown := True;

end.
