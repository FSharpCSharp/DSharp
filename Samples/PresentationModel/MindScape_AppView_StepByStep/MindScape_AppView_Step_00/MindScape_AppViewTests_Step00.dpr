program MindScape_AppViewTests_Step00;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free()
  else
  begin
    Application.Initialize();
    GUITestRunner.RunRegisteredTests();
  end;
end.
