program Calculator;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  CalculatorViewForm in 'CalculatorViewForm.pas' {CalculatorView} ,
  CalculatorViewModel in 'CalculatorViewModel.pas',
  CalculatorInterfaces in 'CalculatorInterfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
  {$ENDIF CodeSite}
  Application.Start<ICalculatorViewModel>();
  ReportMemoryLeaksOnShutdown := True;

end.
