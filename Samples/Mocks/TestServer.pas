unit TestServer;

interface

uses
  TestFramework, Server, DSharp.Testing.Mock;

type
  TestTProtocolServer = class(TTestCase)
  private
    // remember to put the {$M+} on your interface or you will get the "unable to create mock" error later
    FMockServer: Mock<IServer<string, Boolean>>;
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
  FProtocolServer := TProtocolServer.Create(FMockServer);
end;

procedure TestTProtocolServer.TearDown;
begin
  FProtocolServer.Free;
  FProtocolServer := nil;
  FMockServer.Free; // important to clear up the mock here!
end;

procedure TestTProtocolServer.TestCommunicate;
var
  ReturnValue: Boolean;
begin
  // define expectations
  FMockServer.WillReturn(True).Once.WhenCallingWithAnyArguments.SendMessage('');
  FMockServer.WillReturn('This is the message from the server!').Once.WhenCalling.ReceiveMessage;

  ReturnValue := FProtocolServer.Communicate;
  CheckTrue(ReturnValue, 'Communication with the Server Failed');
  FMockServer.Verify; // optional: can check if all expectations were met
end;

initialization
  RegisterTest(TestTProtocolServer.Suite);

end.