program Tests;

uses
  Forms,
  GUITestRunner,
  DSharp.Core.Reflection.Tests in 'DSharp.Core.Reflection.Tests.pas',
  DSharp.Core.Times.Tests in 'DSharp.Core.Times.Tests.pas',
  DSharp.Testing.Mock.Tests in 'DSharp.Testing.Mock.Tests.pas';

{$R *.RES}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

