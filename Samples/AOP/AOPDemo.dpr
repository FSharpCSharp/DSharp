program AOPDemo;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  DSharp.Core.Aspects in '..\..\Source\Core\DSharp.Core.Aspects.pas',
  DSharp.Core.Reflection in '..\..\Source\Core\DSharp.Core.Reflection.pas',
  DSharp.Core.Aspects.Logging in '..\..\Source\Core\DSharp.Core.Aspects.Logging.pas',
  DSharp.Core.Logging in '..\..\Source\Core\DSharp.Core.Logging.pas',
  DSharp.Core.Logging.SmartInspect in '..\..\Source\Core\DSharp.Core.Logging.SmartInspect.pas',
  DSharp.Core.Logging.SmartInspect.Helper in '..\..\Source\Core\DSharp.Core.Logging.SmartInspect.Helper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
