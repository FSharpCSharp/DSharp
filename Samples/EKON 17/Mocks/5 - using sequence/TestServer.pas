unit TestServer;

interface

uses
  TestFramework, Server, DSharp.Testing.Mock;

type
  TProtocolServerTest = class(TTestCase)
  private
    // remember to put the {$M+} on your interface or you will get the "unable to create mock" error later
    FMockServer: Mock<IServer<string, Boolean>>;
    FProtocolServer: TProtocolServer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommunicate;
    procedure TestCommunicate_NotCallingReceiveMessage_When_SendMessageReturnsFalse;
  end;

implementation

uses
  DSharp.Testing.Mock.Sequences;

procedure TProtocolServerTest.SetUp;
begin
  FProtocolServer := TProtocolServer.Create(FMockServer);
end;

procedure TProtocolServerTest.TearDown;
begin
  FProtocolServer.Free;
  FProtocolServer := nil;
  FMockServer.Free; // important to clear up the mock here!
end;

procedure TProtocolServerTest.TestCommunicate;
var
  ReturnValue: Boolean;
  seq: Sequence;
begin
  // define expectations
  with FMockServer.Setup do
  begin
    WillReturn(True)
      .InSequence(seq).Once
      .WhenCallingWithAnyArguments.SendMessage('');

    WillReturn('This is the message from the server!')
      .InSequence(seq).Once
      .WhenCalling.ReceiveMessage();
  end;

  ReturnValue := FProtocolServer.Communicate;

  CheckTrue(ReturnValue, 'Communication with the Server Failed');
  FMockServer.Verify; // optional: can check if all expectations were met
  seq.Verify;
end;

procedure TProtocolServerTest.TestCommunicate_NotCallingReceiveMessage_When_SendMessageReturnsFalse;
var
  ReturnValue: Boolean;
begin
  // define expectations
  with FMockServer.Setup do
  begin
    WillReturn(False).Once.WhenCallingWithAnyArguments.SendMessage('');
    WillExecute.Never.WhenCalling.ReceiveMessage();
  end;

  ReturnValue := FProtocolServer.Communicate;
  CheckFalse(ReturnValue, 'Communication with the Server Failed');
  FMockServer.Verify; // optional: can check if all expectations were met
end;

initialization
  RegisterTest(TProtocolServerTest.Suite);

end.
