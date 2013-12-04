program CompositionFMX;

uses
  FMX.Forms,
  zFormMain in 'zFormMain.pas' {Form1} ,
  zShared in 'zShared.pas',
  zFormFirst in 'zFormFirst.pas' {FormFirst} ,
  zFormSecond in 'zFormSecond.pas' {FormSecond};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
