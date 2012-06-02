unit TestServer;

interface

uses
  TestFramework, Server, MockServer;

type
  TestTProtocolServer = class(TTestCase)
  strict private
    FProtocolServer: TProtocolServer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommunicate;
  end;

implementation

procedure TestTProtocolServer.SetUp;
begin
  FProtocolServer := TProtocolServer.Create(TMockServer.Create);
end;

procedure TestTProtocolServer.TearDown;
begin
  FProtocolServer.Free;
  FProtocolServer := nil;
end;

procedure TestTProtocolServer.TestCommunicate;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FProtocolServer.Communicate;
  CheckTrue(ReturnValue, 'Communication with the Server Failed');
end;

initialization
  RegisterTest(TestTProtocolServer.Suite);

end.
