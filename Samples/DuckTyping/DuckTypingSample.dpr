program DuckTypingSample;

{$APPTYPE CONSOLE}

uses
  DSharp.Core.DuckTyping;

type
  IDuck = interface(IInvokable)
    procedure Quack;
    procedure Feathers;
  end;

  TDuck = class(TInterfacedObject, IDuck)
  public
    procedure Quack;
    procedure Feathers;
  end;

  TPerson = class
  public
    procedure Quack;
    procedure Feathers;
    procedure Name;
  end;

{ TDuck }

procedure TDuck.Feathers;
begin
  Writeln('The duck has white and gray feathers.');
end;

procedure TDuck.Quack;
begin
  Writeln('Quaaaaaack!');
end;

{ TPerson }

procedure TPerson.Feathers;
begin
  Writeln('The person takes a feather from the ground and shows it.');
end;

procedure TPerson.Name;
begin
  Writeln('John Smith');
end;

procedure TPerson.Quack;
begin
  Writeln('The person imitates a duck.');
end;

procedure InTheForest(duck: IDuck);
begin
  duck.quack();
  duck.feathers();
end;

var
  donald: IDuck;
  john: TPerson;
begin
  donald := TDuck.Create;
  john := TPerson.Create;
  InTheForest(donald);
  InTheForest(Duck<IDuck>(john));
  john.Free;
  Readln;
end.
