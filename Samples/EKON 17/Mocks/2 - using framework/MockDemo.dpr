program MockDemo;

uses
  Forms,
  GUITestRunner,
  TestMain in 'TestMain.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

