program PageControlSample;

uses
  Forms,
  MainViewForm in 'MainViewForm.pas' {MainView},
  MainViewModel in 'MainViewModel.pas',
  Interfaces in 'Interfaces.pas' {/  DSharp.PresentationModel.VclApplication;},
  DSharp.PresentationModel.VclApplication,
  DetailViewModel in 'DetailViewModel.pas',
  DetailViewFrame in 'DetailViewFrame.pas' {DetailView: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Start<IMainViewModel>;
  ReportMemoryLeaksOnShutdown := True;
end.
