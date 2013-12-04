program HowToOpenDialog;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  AppViewForm in 'AppViewForm.pas' {AppView} ,
  AppViewModel in 'AppViewModel.pas',
  AppInterfaces in 'AppInterfaces.pas',
  MyDialogViewModel in 'MyDialogViewModel.pas',
  MyDialogViewFrame in 'MyDialogViewFrame.pas' {MyDialogView: TFrame};

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

  // Application.Start<TAppViewModel>();
end.
