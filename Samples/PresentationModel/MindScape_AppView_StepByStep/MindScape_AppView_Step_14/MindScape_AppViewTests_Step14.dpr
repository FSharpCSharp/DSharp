program MindScape_AppViewTests_Step14;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  AppModel in 'AppModel.pas',
  AppViewModelTestCase in 'AppViewModelTestCase.pas',
  AppViewModel in 'AppViewModel.pas',
  AppInterfaces in 'AppInterfaces.pas',
  AppModelTestCase in 'AppModelTestCase.pas';

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
