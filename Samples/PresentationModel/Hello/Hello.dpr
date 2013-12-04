program Hello;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  HelloBootstrapper in 'HelloBootstrapper.pas',
  HelloLogger in 'HelloLogger.pas',
  Interfaces in 'Interfaces.pas',
  zShellView in 'zShellView.pas' {ShellView} ,
  zShellViewModel in 'zShellViewModel.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>;
  {$ENDIF CodeSite}
  // Application.Start<IShellViewModel>();
  // Application.Start<TShellViewModel>();
  Application.Start(THelloBootstrapper);

  // with THelloBootstrapper.Create() do
  // try
  // StartRuntime();
  // finally
  // Free();
  // end;
end.
