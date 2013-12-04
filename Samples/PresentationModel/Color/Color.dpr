// Original sample and blog can be found here:
// http://www.mindscapehq.com/blog/index.php/2012/02/01/caliburn-micro-part-4-the-event-aggregator/
program Color;

uses
  Forms,
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  ColorEvent in 'ColorEvent.pas',
  ColorViewFrame in 'ColorViewFrame.pas' {ColorView: TFrame} ,
  ColorViewModel in 'ColorViewModel.pas',
  Interfaces in 'Interfaces.pas',
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
