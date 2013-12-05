unit DSharp.PresentationModel.Screen;

interface

uses
  SysUtils,
  Rtti,
  DSharp.ComponentModel.Composition,
  DSharp.Core.Events,
  DSharp.Logging,
  DSharp.PresentationModel.ActivateIntf,
  DSharp.PresentationModel.ActivationEventArgsIntf,
  DSharp.PresentationModel.ChildIntf,
  DSharp.PresentationModel.CloseIntf,
  DSharp.PresentationModel.ConductorIntf,
  DSharp.PresentationModel.DeactivateIntf,
  DSharp.PresentationModel.DeactivationEventArgsIntf,
  DSharp.PresentationModel.GuardCloseIntf,
  DSharp.PresentationModel.HaveDisplayNameIntf,
  DSharp.PresentationModel.ScreenIntf,
  DSharp.PresentationModel.ViewAware;

type
  [InheritedExport]
  {$RTTI EXPLICIT METHODS([vcProtected..vcPublished])}
  ///	<summary>
  ///	  A base implementation of <see cref="IScreen" />.
  ///	</summary>
  TScreen = class(TViewAware, IScreen, IChild, IHaveDisplayName, IActivate,
    IDeactivate, IClose, IGuardClose)
  strict private
    FActivated: Event<TActivationEvent>;
    FAttemptingDeactivation: Event<TDeactivationEvent>;
    FDeactivated: Event<TDeactivationEvent>;
    FDisplayName: string;
    FIsActive: Boolean;
    FIsInitialized: Boolean;
    FModalResult: TModalResult;
    FParent: TValue;
    class var FLog: ILog;
    class function GetLog: ILog; static;
    function GetViewCloseAction(ModalResult: TModalResult = mrNone): TProc;
    procedure SetIsInitialized(const Value: Boolean);
    procedure SetModalResult(const Value: TModalResult);
  protected
    ///	<summary>
    ///	  Called when activating.
    ///	</summary>
    procedure OnActivate(); virtual;

    ///	<summary>
    ///	  Called when deactivating.
    ///	</summary>
    ///	<param name="Close">
    ///	  Inidicates whether this instance will be closed.
    ///	</param>
    procedure OnDeactivate(Close: Boolean); virtual;

    ///	<summary>
    ///	  Called when initializing.
    ///	</summary>
    procedure OnInitialize(); virtual;

    class property Log: ILog read GetLog;
  public
    ///	<summary>
    ///	  Creates an instance of the screen.
    ///	</summary>
    constructor Create; override;
  public
    {$REGION 'Implements IChild'}
    function GetParent: TValue; virtual;
    procedure SetParent(const Value: TValue); virtual;

    ///	<summary>
    ///	  Gets or Sets the Parent <see cref="IConductor" />
    ///	</summary>
    property Parent: TValue read GetParent write SetParent;
    {$ENDREGION}
    {$REGION 'Implements IHaveDisplayName'}
    function GetDisplayName: string; virtual;
    procedure SetDisplayName(const Value: string); virtual;

    ///	<summary>
    ///	  Gets or Sets the Display Name
    ///	</summary>
    property DisplayName: string read GetDisplayName write SetDisplayName;
    {$ENDREGION}
    {$REGION 'Implements IActivate'}

    ///	<summary>
    ///	  Activates this instance.
    ///	</summary>
    procedure Activate;

    ///	<summary>
    ///	  Called to check whether or not this instance can close.
    ///	</summary>
    ///	<param name="callback">
    ///	  The implementer calls this action with the result of the close check.
    ///	</param>
    function GetActivated: IEvent<TActivationEvent>;
    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);

    ///	<summary>
    ///	  Raised after activation occurs.
    ///	</summary>
    property Activated: IEvent<TActivationEvent> read GetActivated;

    ///	<summary>
    ///	  Indicates whether or not this instance is currently active.
    ///	</summary>
    property IsActive: Boolean read GetIsActive;
    {$ENDREGION}
    {$REGION 'Implements IDeactivate'}

    ///	<summary>
    ///	  Deactivates this instance.
    ///	</summary>
    ///	<param name="Close">
    ///	  Indicates whether or not this instance is being closed.
    ///	</param>
    procedure Deactivate(Close: Boolean);
    function GetAttemptingDeactivation: IEvent<TDeactivationEvent>;
    function GetDeactivated: IEvent<TDeactivationEvent>;

    ///	<summary>
    ///	  Raised before deactivation.
    ///	</summary>
    property AttemptingDeactivation: IEvent<TDeactivationEvent>
      read GetAttemptingDeactivation;

    ///	<summary>
    ///	  Raised after deactivation.
    ///	</summary>
    property Deactivated: IEvent<TDeactivationEvent> read GetDeactivated;
    {$ENDREGION}
    {$REGION 'Implements IClose'}
    function GetModalResult: TModalResult;
    procedure TryClose(ModalResult: TModalResult = mrNone); virtual;

    ///	<summary>
    ///	  TModalResult represents the return value from a modal dialog.
    ///	</summary>
    property ModalResult: TModalResult read GetModalResult;
    {$ENDREGION}
    {$REGION 'Implements IGuardClose'}
    procedure CanClose(Callback: TProc<Boolean>); virtual;
    {$ENDREGION}

    ///	<summary>
    ///	  Indicates whether or not this instance is currently initialized.
    ///	</summary>
    property IsInitialized: Boolean read FIsInitialized;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.ActivationEventArgs,
  DSharp.PresentationModel.DeactivationEventArgs,
  DSharp.PresentationModel.Execute;

{ TScreen }

constructor TScreen.Create;
begin
  inherited Create;
  FDisplayName := ClassType.ClassName;
  FIsActive := False;
  FIsInitialized := False;
  FModalResult := mrNone;
  FParent := nil;
end;

procedure TScreen.Activate;
var
  LInitialized: Boolean;
begin
  if IsActive then
    Exit;

  LInitialized := False;

  if not IsInitialized then
  begin
    SetIsInitialized(True);
    LInitialized := True;
    OnInitialize();
  end;

  SetIsActive(True);
  Log.LogMessage('Activating %s.', [Self]);
  OnActivate();

  Activated.Invoke(Self, TActivationEventArgs.Create(LInitialized)
    as IActivationEventArgs);
end;

procedure TScreen.CanClose(Callback: TProc<Boolean>);
begin
  Callback(True);
end;

procedure TScreen.Deactivate(Close: Boolean);
begin
  if IsActive or (IsInitialized and Close) then
  begin
    AttemptingDeactivation.Invoke(Self, TDeactivationEventArgs.Create(Close)
      as IDeactivationEventArgs);

    SetIsActive(False);
    Log.LogMessage('Deactivating %s.', [Self]);
    OnDeactivate(Close);

    Deactivated.Invoke(Self, TDeactivationEventArgs.Create(Close)
      as IDeactivationEventArgs);

    if Close then
    begin
      Views.Clear;
      Log.LogMessage('Closed %s.', [Self]);
    end;
  end;
end;

function TScreen.GetActivated: IEvent<TActivationEvent>;
begin
  Result := FActivated;
end;

function TScreen.GetAttemptingDeactivation: IEvent<TDeactivationEvent>;
begin
  Result := FAttemptingDeactivation;
end;

function TScreen.GetDeactivated: IEvent<TDeactivationEvent>;
begin
  Result := FDeactivated;
end;

function TScreen.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TScreen.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

class function TScreen.GetLog: ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(TScreen));
  end;
  Result := FLog;
end;

function TScreen.GetModalResult: TModalResult;
begin
  Result := FModalResult;
end;

function TScreen.GetParent: TValue;
begin
  Result := FParent;
end;

function TScreen.GetViewCloseAction(ModalResult: TModalResult): TProc;
var
  LConductor: IConductor;
  LContextualView: TObject;
  LViewType: TRttiType;
  LCloseMethod: TRttiMethod;
  LIsClosed: Boolean;
  LModalResultProperty: TRttiProperty;
begin
  if Supports(Parent, IConductor, LConductor) then
  begin
    Exit(
      procedure
      begin
        LConductor.DeactivateItem(Self, True);
      end);
  end;

  for LContextualView in Views.Values do
  begin
    LViewType := GetRttiType(LContextualView.ClassType);
    LCloseMethod := LViewType.GetMethod('Close');

    if Assigned(LCloseMethod) then
      Exit(
        procedure
        begin
          LIsClosed := False;

          if ModalResult > mrNone then
          begin
            LModalResultProperty := LViewType.GetProperty('ModalResult');
            if Assigned(LModalResultProperty) then
            begin
              LModalResultProperty.SetValue(LContextualView, ModalResult);
              LIsClosed := True;
            end;
          end;

          if not LIsClosed then
          begin
            LCloseMethod.Invoke(LContextualView, []);
          end;
        end);
  end;

  Exit(
    procedure
    begin
      Log.LogMessage
        ('TryClose requires a parent IConductor or a view with a Close method or IsOpen property.');
    end);
end;

procedure TScreen.OnActivate;
begin
end;

procedure TScreen.OnDeactivate(Close: Boolean);
begin
end;

procedure TScreen.OnInitialize;
begin
end;

procedure TScreen.SetDisplayName(const Value: string);
begin
  if Value <> FDisplayName then
  begin
    FDisplayName := Value;
    NotifyOfPropertyChange('DisplayName');
  end;
end;

procedure TScreen.SetIsActive(const Value: Boolean);
begin
  if Value <> FIsActive then
  begin
    FIsActive := Value;
    NotifyOfPropertyChange('IsActive');
  end;
end;

procedure TScreen.SetIsInitialized(const Value: Boolean);
begin
  if Value <> FIsInitialized then
  begin
    FIsInitialized := Value;
    NotifyOfPropertyChange('IsInitialized');
  end;
end;

procedure TScreen.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
end;

procedure TScreen.SetParent(const Value: TValue);
begin
  if not SameValue(Value, FParent) then
  begin
    FParent := Value;
    NotifyOfPropertyChange('Parent');
  end;
end;

procedure TScreen.TryClose(ModalResult: TModalResult);
begin
  Execute.OnUIThread(
    procedure
    var
      LCloseAction: TProc;
    begin
      LCloseAction := GetViewCloseAction(ModalResult);
      LCloseAction();
    end);
end;

end.
