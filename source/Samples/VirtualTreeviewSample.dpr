program VirtualTreeviewSample;

uses
  Forms,
  VirtualTreeviewSample.Main in 'VirtualTreeviewSample.Main.pas' {MainForm},
  Sample5.Contact in 'Sample5.Contact.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
