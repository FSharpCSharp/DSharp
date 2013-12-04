program MindScape_AppViewTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  AppViewModelTestCase in 'AppViewModelTestCase.pas',
  AppViewModel in 'AppViewModel.pas',
  AppModel in 'AppModel.pas';

{$R *.res}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free()
  else
    GUITestRunner.RunRegisteredTests;

end.
