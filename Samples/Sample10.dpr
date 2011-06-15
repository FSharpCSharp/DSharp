program Sample10;

{$APPTYPE CONSOLE}

uses
  Sample10.Main,
  Sample10.Message,
  DSharp.ComponentModel.Composition.Catalog,
  DSharp.ComponentModel.Composition.Container,
  SysUtils;

var
  main: TMain;
  catalog: TRttiCatalog;
  container: TCompositionContainer;
begin
  ReportMemoryLeaksOnShutdown := True;
  main := TMain.Create;
  catalog := TRttiCatalog.Create();
  container := TCompositionContainer.Create(catalog);
  try
    try
      container.SatisfyImportsOnce(main);
      main.Run();
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    container.Free();
    catalog.Free();
    main.Free;
  end;
  Readln;
end.
