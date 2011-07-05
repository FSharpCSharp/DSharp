program Tests;

uses
  Forms,
  GUITestRunner,
  DSharp.Core.Reflection.Testing in 'DSharp.Core.Reflection.Testing.pas';

{$R *.RES}

begin
  Application.Initialize;
  RunRegisteredTests;
end.

