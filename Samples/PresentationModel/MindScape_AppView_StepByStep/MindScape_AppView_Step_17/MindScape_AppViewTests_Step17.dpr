program MindScape_AppViewTests_Step17;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  AppInterfaces in 'AppInterfaces.pas',
  AppModel in 'AppModel.pas',
  AppModelTestCase in 'AppModelTestCase.pas',
  AppViewModelTestCase in 'AppViewModelTestCase.pas',
  AppViewModel in 'AppViewModel.pas';

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
