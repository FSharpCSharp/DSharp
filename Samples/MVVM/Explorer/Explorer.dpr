program Explorer;

uses
  DSharp.PresentationModel.VCLApplication,
  Forms,
  MainViewForm in 'MainViewForm.pas' {MainView},
  Interfaces in 'Interfaces.pas',
  MainViewModel in 'MainViewModel.pas',
  NavigationViewFrame in 'NavigationViewFrame.pas' {NavigationView: TFrame},
  NavigationViewModel in 'NavigationViewModel.pas',
  WorkingAreaFrame in 'WorkingAreaFrame.pas' {WorkingAreaView: TFrame},
  WorkingAreaViewModel in 'WorkingAreaViewModel.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Start<IMainViewModel>;
end.
