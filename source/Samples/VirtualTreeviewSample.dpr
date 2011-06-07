program VirtualTreeviewSample;

uses
  Forms,
  VirtualTreeviewSample.Main in 'VirtualTreeviewSample.Main.pas' {Main},
  Sample5.Contact in 'Sample5.Contact.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
