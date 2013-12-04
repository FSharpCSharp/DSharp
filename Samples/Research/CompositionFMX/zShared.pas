unit zShared;

interface

uses
  Classes,
  SysUtils,
  FMX.Types;

type
  TManager = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  ILogger = interface
    ['{B342CCF5-2F91-41A5-9BB3-378CE0490F3D}']
    procedure LogInfo(Msg: string);
  end;

  TLogAddedEvent = procedure(Sender: TObject; Value: string) of object;

  TLogger = class(TInterfacedObject, ILogger)
  private
    FOnLog: TLogAddedEvent;
  public
    procedure LogInfo(Msg: string);
    property OnLog: TLogAddedEvent read FOnLog write FOnLog;
  end;

procedure SetContentPropertyCore(TargetLocation, View: TObject);

var
  Manager: TManager;
  Logger: ILogger;

implementation

uses
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts;

{ TLogger }

procedure TLogger.LogInfo(Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Msg);
end;

{ TManager }

procedure TManager.Notification(AComponent: TComponent; Operation: TOperation);
  function GetName(AComponent: TComponent): string;
  begin
    if Assigned(AComponent) then
      Result := Format('%s<%s>', [AComponent.Name, AComponent.ClassName])
    else
      Result := 'null';
  end;

begin
  inherited;
  case Operation of
    opInsert:
      Logger.LogInfo(Format('Manager Insert %s Owner is %s', [GetName(AComponent), GetName(AComponent.Owner)]));
    opRemove:
      Logger.LogInfo(Format('Manager Remove %s Owner is %s', [GetName(AComponent), GetName(AComponent.Owner)]));
  end;
end;

procedure SetContentPropertyCore(TargetLocation, View: TObject);
var
  LForm: TForm;
  LView: TFmxObject;
  LTargetLocation: TControl;
begin
  LTargetLocation := (TargetLocation as TControl);

  // Remove all children from target location
  while LTargetLocation.ChildrenCount > 0 do
  begin
    LTargetLocation.Children[0].Parent := nil;
  end;

  // Insert view into target location
  if Assigned(View) then
  begin
    // TForm is the host for the actual view
    LForm := View as TForm;
    // Set TForm's parent to TargentLocation so it will be destroyed when TargetLocation gets destroyed
    LForm.Parent := LTargetLocation;
    // The actual view is the first and only child (TLayout) of TForm
    LView := LForm.Components[0] as TLayout;
    // This will show control insice target location
    LView.Parent := LTargetLocation;
  end;
end;

initialization

Manager := TManager.Create(nil);

Logger := TLogger.Create;

finalization

Manager.Free;

Logger := nil;

end.
