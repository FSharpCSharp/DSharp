unit BusyResult;

interface

uses
  DSharp.PresentationModel,
  Forms,
  DSharp.Core.Reflection;

type
  TBusyResult = class(TResultBase)
  private
    FHide: Boolean;
  public
    constructor Create(Hide: Boolean);
    procedure Execute(Context: IActionExecutionContext); override;
  end;

implementation

uses
  DSharp.Core.Framework;

{ TBusyResult }

constructor TBusyResult.Create(Hide: Boolean);
begin
  FHide := Hide;
end;

procedure TBusyResult.Execute(Context: IActionExecutionContext);
var
  LView: TComponent;
  LBusyIndicator: TComponent;
begin
  LView := Context.View as TComponent;

  // Search up the visual tree
  while Assigned(LView) do
  begin
    LBusyIndicator := LView.FindComponent('BusyIndicator') as TComponent;
    if Assigned(LBusyIndicator) then
    begin
      if FHide then
      begin
        LBusyIndicator.GetType.GetProperty('Visible')
          .SetValue(LBusyIndicator, False);
        LBusyIndicator.GetType.GetMethod('SendToBack')
          .Invoke(LBusyIndicator, []);
      end
      else
      begin
        LBusyIndicator.GetType.GetMethod('BringToFront')
          .Invoke(LBusyIndicator, []);
        LBusyIndicator.GetType.GetProperty('Visible')
          .SetValue(LBusyIndicator, True);
      end;
      Application.ProcessMessages;
      Break;
    end;
    LView := TFramework.GetParent(LView)
  end;

  Completed.Invoke(Self, TResultCompletionEventArgs.Create);
end;

end.
