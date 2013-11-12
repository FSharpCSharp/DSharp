program MockDemo;

uses
  Forms,
  GUITestRunner,
  TestMain in 'TestMain.pas',
  LoggerIntf in 'LoggerIntf.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

