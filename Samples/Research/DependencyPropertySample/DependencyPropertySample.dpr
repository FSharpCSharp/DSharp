program DependencyPropertySample;

uses
  DSharp.PresentationModel.VCLApplication,
  Forms,
  Main in 'Source\Main.pas' {MainForm} ,
  SimpleViewFrame in 'Source\SimpleViewFrame.pas' {SimpleView: TFrame} ,
  Themes,
  ViewExtensions in 'Source\ViewExtensions.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
