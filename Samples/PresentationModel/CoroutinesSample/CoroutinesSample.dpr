program CoroutinesSample;

uses
  DSharp.PresentationModel.VCLApplication,
  Forms,
  Interfaces in 'Source\Interfaces.pas',
  ShowMessage in 'Source\ShowMessage.pas',
  zMainView in 'Source\zMainView.pas' {MainView} ,
  zMainViewModel in 'Source\zMainViewModel.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Start<IMainViewModel>();

end.
