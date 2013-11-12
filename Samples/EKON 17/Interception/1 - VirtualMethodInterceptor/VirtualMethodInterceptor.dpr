program VirtualMethodInterceptor;

{$APPTYPE CONSOLE}

uses
  Rtti,
  SysUtils;

type
  TGreeting = class
    function SayHelloWorld: Boolean; virtual;
  end;

{ TGreeting }

function TGreeting.SayHelloWorld: Boolean;
begin
  Writeln('Hello World');
  Result := True;
//  raise Exception.Create('some error');
end;

procedure DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
begin
  Writeln('before method: ', Method.Name);
end;

procedure DoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
begin
  Writeln('after method: ', Method.Name);
end;

procedure DoException(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out RaiseException: Boolean;
  TheException: Exception; out Result: TValue);
begin
  Writeln('an exception occured while executing method: ', Method.Name);
  Writeln('with the message: ', TheException.Message);
  RaiseException := False;
  Result := False;
end;

procedure Main;
var
  vmi: TVirtualMethodInterceptor;
  greeting: TGreeting;
begin
  greeting := TGreeting.Create;

//  vmi := TVirtualMethodInterceptor.Create(TGreeting);
//  vmi.OnBefore := DoBefore;
//  vmi.OnAfter := DoAfter;
//  vmi.OnException := DoException;
//  vmi.Proxify(greeting);

  if not greeting.SayHelloWorld then
    Writeln('greeting did not work');
  Writeln('class: ', greeting.ClassName);
  Writeln('parent class: ', greeting.ClassParent.ClassName);
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
