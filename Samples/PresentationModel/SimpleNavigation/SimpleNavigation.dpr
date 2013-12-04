program SimpleNavigation;

uses
  Forms,
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Interfaces in 'Interfaces.pas',
  PageOneViewFrame in 'PageOneViewFrame.pas' {PageOneView: TFrame} ,
  PageOneViewModel in 'PageOneViewModel.pas',
  PageTwoViewFrame in 'PageTwoViewFrame.pas' {PageTwoView: TFrame} ,
  PageTwoViewModel in 'PageTwoViewModel.pas',
  ShellViewForm in 'ShellViewForm.pas' {ShellView} ,
  ShellViewModel in 'ShellViewModel.pas';

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
