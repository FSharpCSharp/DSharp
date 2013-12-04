program MindScape_AppViewVCL;

uses
  {$ifdef CodeSite}
  DSharp.Logging.CodeSite,
  {$endif CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  AppInterfaces in 'AppInterfaces.pas',
  AppModel in 'AppModel.pas',
  AppViewForm in 'AppViewForm.pas' {AppView},
  AppViewModel in 'AppViewModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
{$ifdef DEBUG}
  Application.WithDebugLogger();
{$endif DEBUG}
{$ifdef CodeSite}
  Application.WithLogger<TCodeSiteLog>;
{$endif CodeSite}
  Application.Start<IAppViewModel>();
//  Application.Start<TAppViewModel>();
end.
