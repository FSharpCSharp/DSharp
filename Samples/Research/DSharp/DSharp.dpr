program DSharp;

{$APPTYPE CONSOLE}
{$R *.res}
{$R *.dres}

uses
  SysUtils,
  ApplicationCreator in 'Source\ApplicationCreator.pas',
  Interfaces in 'Source\Interfaces.pas',
  ModuleCreator in 'Source\ModuleCreator.pas',
  SwitchProcessor in 'Source\SwitchProcessor.pas',
  TemplateEngine in 'Source\TemplateEngine.pas',
  ModelCreator in 'Source\ModelCreator.pas',
  Settings in 'Source\Settings.pas';

var
  Value: string;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if ParamCount = 0 then
    begin
      Writeln('Usage:');
      Writeln;
      Writeln('Example: Create an application named ContactManager with modules Shell, Contact and models Address, Contact');
      Writeln('>> dsharp /app ContactManager /module Shell,Contact /model Address,Contact');
    end;

    if FindCmdLineSwitch('app', Value, True) then
    begin
      TSwitchProcessor.ProcessApplication(Value);
    end;

    if FindCmdLineSwitch('module', Value, True) then
    begin
      TSwitchProcessor.ProcessModules(Value);
    end;

    if FindCmdLineSwitch('model', Value, True) then
    begin
      TSwitchProcessor.ProcessModels(Value);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
