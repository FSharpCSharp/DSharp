program Tests;

uses
  Forms,
  GUITestRunner,
  DSharp.Core.Events.Tests in 'DSharp.Core.Events.Tests.pas',
  DSharp.Core.Reflection.Tests in 'DSharp.Core.Reflection.Tests.pas',
  DSharp.Core.Times.Tests in 'DSharp.Core.Times.Tests.pas';

{$R *.RES}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  RunRegisteredTests;
end.
