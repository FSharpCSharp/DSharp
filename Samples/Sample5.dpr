program Sample5;

uses
  Forms,
  Sample5.Main in 'Sample5.Main.pas' {MainForm},
  Sample5.Contact in 'Sample5.Contact.pas',
  Sample5.Contact.Template in 'Sample5.Contact.Template.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
