unit DSharp.Testing.Mock.DUnit;

interface

implementation

uses
  DSharp.Testing.DUnit,
  DSharp.Testing.Mock.Internals;

type
  TMockIntegration = class
  private class threadvar
    FCurrentTestCase: TTestCase;
  protected
    class procedure HandleVerify(Sender: TObject);
    class procedure HandleInvoke(Sender: TObject);
  public
    class procedure Initialize; static;
  end;

class procedure TMockIntegration.HandleVerify(Sender: TObject);
begin
  FCurrentTestCase.Check(True);
end;

class procedure TMockIntegration.HandleInvoke(Sender: TObject);
begin
  FCurrentTestCase := Sender as TTestCase;
end;

class procedure TMockIntegration.Initialize;
begin
  TTestCase.OnInvoke := HandleInvoke;
  TMock.OnVerify := HandleVerify;
end;

initialization
  TMockIntegration.Initialize;

end.
