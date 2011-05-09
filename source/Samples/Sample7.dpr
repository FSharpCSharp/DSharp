program Sample7;

uses
  Forms,
  Sample7.Main in 'Sample7.Main.pas' {Form3},
  Sample7.Customer in 'Sample7.Customer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
