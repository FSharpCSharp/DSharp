program Sample10;

{$APPTYPE CONSOLE}
//{$STRONGLINKTYPES ON}

uses
  Sample10.Main,
  Sample10.Message,
  DSharp.ComponentModel.Composition.Catalog,
  DSharp.ComponentModel.Composition.Container,
  DSharp.ComponentModel.Composition.Primitives,
  SysUtils;

var
  main: TMain;
  catalog: TBaseCatalog;
  container: TCompositionContainer;
begin
  ReportMemoryLeaksOnShutdown := True;
  catalog := TRttiCatalog.Create();
  container := TCompositionContainer.Create(catalog);

  try
    try
      main := container.Resolve<TMain>;
      main.Run();
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    container.Free();
    catalog.Free();
  end;
  Readln;
end.
