program EventAggregatorSample;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  Commands in 'Source\Commands.pas',
  zFrameA in 'Source\zFrameA.pas' {FrameA: TFrame} ,
  zFrameB in 'Source\zFrameB.pas' {FrameB: TFrame} ,
  zUnit1 in 'Source\zUnit1.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize();
  Application.MainFormOnTaskbar := True;
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
  {$ENDIF CodeSite}
  Application.CreateForm(TForm1, Form1);
  Application.Run();

end.
