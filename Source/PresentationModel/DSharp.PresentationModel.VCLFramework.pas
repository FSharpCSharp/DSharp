unit DSharp.PresentationModel.VCLFramework;

interface

uses
  Classes,
  DSharp.Core.Framework;

type
  TPresentationFramework = class(TFramework)
  protected
    class function DoGetChildren(Element: TComponent)
      : TArray<TComponent>; override;
    class function DoGetParent(Element: TComponent): TComponent; override;
    class procedure DoNotifyPropertyChanged(Element: TComponent;
      const PropertyName: string); override;
  end;

implementation

uses
  Actions,
  Controls,
  DSharp.Bindings;

{ TPresentationFramework }

class function TPresentationFramework.DoGetChildren(Element: TComponent)
  : TArray<TComponent>;
var
  i: Integer;
begin
  Result := nil;
  if Element is TWinControl then
  begin
    SetLength(Result, TWinControl(Element).ControlCount);
    for i := 0 to TWinControl(Element).ControlCount - 1 do
    begin
      Result[i] := TWinControl(Element).Controls[i];
    end;
  end;
end;

class function TPresentationFramework.DoGetParent(Element: TComponent)
  : TComponent;
begin
  if Element is TControl then
  begin
    Result := TControl(Element).Parent;
  end
  else if Element is TBasicAction then
  begin
    Result := TBasicAction(Element).Owner;
  end
  else
  begin
    Result := nil;
    // into the `else` so it is possible to put a breakpoint on it.
  end;
  { TODO -o##jwp -cExtend : Handle `TMenuItem`; see `function TMenuItem.GetHandle: HMENU;` }
end;

class procedure TPresentationFramework.DoNotifyPropertyChanged
  (Element: TComponent; const PropertyName: string);
begin
  DSharp.Bindings.NotifyPropertyChanged(Element, PropertyName);
end;

initialization

TPresentationFramework.Initialize;

end.
