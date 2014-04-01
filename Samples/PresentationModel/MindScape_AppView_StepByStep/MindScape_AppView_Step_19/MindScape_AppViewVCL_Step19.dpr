program MindScape_AppViewVCL_Step19;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  AppInterfaces in 'AppInterfaces.pas',
  AppModel in 'AppModel.pas',
  AppViewForm in 'AppViewForm.pas' {AppView},
  AppViewModel in 'AppViewModel.pas',
  IntegerToRoundedFloatConverter in '..\MindScape_AppView_Step_17\IntegerToRoundedFloatConverter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF DEBUG}
  Application.WithDebugLogger();
{$ENDIF DEBUG}
{$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>;
{$ENDIF CodeSite}
  Application.Start<IAppViewModel>();
end.
