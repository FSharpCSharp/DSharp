unit Server;

interface

type
  {$M+}
  IServer<T, TResult> = interface
    ['{61EA6E1F-49A0-44AF-832C-5C15F492E981}']
    function SendMessage(arg: T): TResult;
    function ReceiveMessage: T;
  end;

  TServer = class(TInterfacedObject, IServer<string, Boolean>)
  private
    function SendMessage(msg : string): Boolean;
    function ReceiveMessage: string;
  end;

  TProtocolServer = class(TObject)
  private
    FServer: IServer<String, Boolean>;
    procedure SetServer(Value: IServer<String, Boolean>);
  public
    constructor Create(Server : IServer<String, Boolean>);
    function Communicate: Boolean;
    property Server : IServer<String, Boolean> read FServer write SetServer;
  end;

implementation

{ TServer }

function TServer.ReceiveMessage: string;
begin
  Result := 'This is the message from the server!';
end;

function TServer.SendMessage(msg: string): Boolean;
begin
  Result :=  msg = 'Message from Client';
end;

{ TProtocolServer }

function TProtocolServer.Communicate : Boolean;
begin
  Result := False;
  if FServer.SendMessage('Message from Client') then
    Result := FServer.ReceiveMessage() = 'This is the message from the server!';
end;

constructor TProtocolServer.Create(Server: IServer<string, Boolean>);
begin
  SetServer(Server);
end;

procedure TProtocolServer.SetServer(Value: IServer<string, Boolean>);
begin
  FServer := Value;
end;

end.
