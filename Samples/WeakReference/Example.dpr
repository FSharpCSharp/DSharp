program Example;

{$APPTYPE CONSOLE}

uses
  SysUtils, Main;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    ChildParent;
    DoubleLinkedList;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
