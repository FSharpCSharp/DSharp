program VirtualInterface;

{$APPTYPE CONSOLE}

uses
  Rtti,
  DSharp.Core.VirtualInterface,
  SysUtils;

type
  IGreeting = interface(IInvokable)
  ['{1C39DAB4-335B-4B45-A48B-C7CC5B450FF4}']
    function SayHelloWorld: Boolean;
  end;

  TGreeting = class(TInterfacedObject, IGreeting)
    function SayHelloWorld: Boolean;
  end;

{ TGreeting }

function TGreeting.SayHelloWorld: Boolean;
begin
  Writeln('Hello World');
  Result := True;
//  raise Exception.Create('some error');
end;

procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
  out Result: TValue);
begin
  Writeln('Hallo Welt');
  Result := True;
end;

procedure Main;
var
  greeting: IGreeting;
  intf: TVirtualInterface;
begin
  intf := TVirtualInterface.Create(TypeInfo(IGreeting), DoInvoke);
  if not Supports(intf, IGreeting, greeting) then
  begin
    Writeln('something did not work');
    Exit;
  end;

  if not greeting.SayHelloWorld then
    Writeln('greeting did not work');
end;

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
