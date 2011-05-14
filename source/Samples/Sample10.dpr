program Sample10;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  System.ComponentModel.Composition.Catalog,
  System.ComponentModel.Composition.Container,
  Sample10.Message,
  Sample10.Main;

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
      main.Run;
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
