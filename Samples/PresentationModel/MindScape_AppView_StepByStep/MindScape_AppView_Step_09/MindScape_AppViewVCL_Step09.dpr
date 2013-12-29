program MindScape_AppViewVCL_Step09;

uses
{$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
{$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  AppInterfaces in 'AppInterfaces.pas',
  AppViewForm in 'AppViewForm.pas' {AppView} ,
  AppViewModel in 'AppViewModel.pas';

{$R *.res}

begin
  Application.Initialize();
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF DEBUG}
  Application.WithDebugLogger();
{$ENDIF DEBUG}
{$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
{$ENDIF CodeSite}
  Application.Start<IAppViewModel>();
end.
