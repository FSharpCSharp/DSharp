program Calculator;

uses
  Forms,
  CalculatorViewForm in 'CalculatorViewForm.pas' {CalculatorView},
  CalculatorViewModel in 'CalculatorViewModel.pas',
  CalculatorInterfaces in 'CalculatorInterfaces.pas',
  DSharp.PresentationModel.VCLApplication;

{$R *.res}

begin
  Application.Initialize;
  Application.Start<ICalculatorViewModel>;
  ReportMemoryLeaksOnShutdown := True;
end.
