program HowToSetFocus;

uses
  Forms,
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Interfaces in 'Interfaces.pas',
  FocusExtension in 'Framework\FocusExtension.pas',
  ShellViewModel in 'ViewModels\ShellViewModel.pas',
  ShellViewForm in 'Views\ShellViewForm.pas' {ShellView};

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
  {$ENDIF CodeSite}
  Application.Start<IShellViewModel>();
  ReportMemoryLeaksOnShutdown := True;

end.
