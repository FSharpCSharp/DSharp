unit MockServer;

interface

uses
  Server;

type
  TMockServer = class(TServer, IServer<string, Boolean>)
  public
    function SendMessage(msg : string): Boolean;
    function ReceiveMessage: string;
  end;

implementation

{ TMockService }

function TMockServer.ReceiveMessage: string;
begin
  Result := 'This is the message from the server!';
end;

function TMockServer.SendMessage(msg: string): Boolean;
begin
  Result := True;
end;

end.
