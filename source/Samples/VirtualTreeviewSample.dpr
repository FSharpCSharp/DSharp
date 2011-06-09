program VirtualTreeviewSample;

uses
  Forms,
  VirtualTreeviewSample.Main in 'VirtualTreeviewSample.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
