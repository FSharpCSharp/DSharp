program MindScape_AppViewFMX_Step16;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.FMXApplication,
  FMX.Forms,
  AppInterfaces in 'AppInterfaces.pas',
  AppModel in 'AppModel.pas',
  AppViewFormFmx in 'AppViewFormFmx.pas' {AppView},
  AppViewModel in 'AppViewModel.pas';

{$R *.res}

begin
  Application.Initialize();
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF DEBUG}
  Application.WithDebugLogger();
{$ENDIF DEBUG}
{$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
{$ENDIF CodeSite}
  Application.Start<IAppViewModel>();
end.
