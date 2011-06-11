program Sample7;

uses
  Forms,
  Sample7.Main in 'Sample7.Main.pas' {MainForm},
  Sample7.Customer in 'Sample7.Customer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
