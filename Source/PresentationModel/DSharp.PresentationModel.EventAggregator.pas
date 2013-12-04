unit DSharp.PresentationModel.EventAggregator;

interface

uses
  SysUtils,
  Rtti,
  TypInfo,
  Generics.Collections,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.Core.Reflection,
  DSharp.Logging,
  DSharp.PresentationModel.EventAggregatorIntf;

type

  ///	<summary>
  ///	  Enables loosely-coupled publication of and subscription to events.
  ///	</summary>
  [PartCreationPolicy(cpShared)]
  TEventAggregator = class(TInterfacedObject, IEventAggregator)
  type
    THandler = class
    strict private
      FReference: TObject;
      FSupportedHandlers: TDictionary<PTypeInfo, TRttiMethod>;
      class var FLog: ILog;

      ///	<summary>
      ///	  Returns all potential handler methods
      ///	</summary>
      function GetHandleMethods(AType: TClass)
        : TDictionary<PTypeInfo, TRttiMethod>;
      class function GetLog(): ILog; static;
    protected
      class property Log: ILog read GetLog;
    public
      constructor Create(Handler: TObject);
      destructor Destroy; override;
      function Handle(MessageType: PTypeInfo; AMessage: TValue): Boolean;
      function Matches(Instance: TObject): Boolean;
      function Handles(MessageType: PTypeInfo): Boolean;
    end;
  private
    FHandlers: TList<THandler>;
  public


    ///	<summary>
    ///	  Processing of handler results on publication thread.
    ///	</summary>
    class var HandlerResultProcessing: TProc<TObject, TValue>;

    ///	<summary>
    ///	  Initializes a new instance of the <see cref="EventAggregator" />class.
    ///
    ///	</summary>
    constructor Create;
    class constructor Create;
    destructor Destroy; override;

    ///	<summary>
    ///	  Searches the subscribed handlers to check if we have a handler for
    ///	  the message type supplied.
    ///	</summary>
    ///	<param name="MessageType">
    ///	  The message type to check with
    ///	</param>
    ///	<returns>
    ///	  True if any handler is found, false if not.
    ///	</returns>
    function HandlerExistsFor(MessageType: PTypeInfo): Boolean;

    ///	<summary>
    ///	  Publishes a message.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    ///	<param name="AMarshal">
    ///	  Allows the publisher to provide a custom thread marshaller for the
    ///	  message publication.
    ///	</param>
    procedure Publish(AMessage: TValue; Marshal: TProc<TProc>;
      AutoFree: Boolean = True); overload; virtual;

    ///	<summary>
    ///	  Publishes a message on the current thread (synchronous).
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure PublishOnCurrentThread(const AMessage: TValue);

    ///	<summary>
    ///	  Publishes a message on a background thread (async).
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure PublishOnBackgroundThread(const AMessage: TValue);

    ///	<summary>
    ///	  Publishes a message on the UI thread.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure PublishOnUIThread(const AMessage: TValue);

    ///	<summary>
    ///	  Publishes a message on the UI thread (asynchronous) without waiting
    ///	  for it to complete.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure BeginPublishOnUIThread(const AMessage: TValue);

    ///	<summary>
    ///	  Subscribes an instance to all events declared through implementations
    ///	  of <see cref="IHandle{T}" />
    ///	</summary>
    ///	<param name="Subscriber">
    ///	  The instance to subscribe for event publication.
    ///	</param>
    procedure Subscribe(Subscriber: TObject); virtual;

    ///	<summary>
    ///	  Unsubscribes the instance from all events.
    ///	</summary>
    ///	<param name="Subscriber">
    ///	  The instance to unsubscribe.
    ///	</param>
    procedure Unsubscribe(Subscriber: TObject); virtual;
  end;

implementation

uses
  Classes,
  Spring,
  Spring.Reflection,
  DSharp.PresentationModel.INPC;

constructor TEventAggregator.Create;
begin
  FHandlers := TObjectList<THandler>.Create(True);
end;

procedure TEventAggregator.BeginPublishOnUIThread(const AMessage: TValue);
begin
  Publish(AMessage, Execute.QueueActionOnUIThread);
end;

class constructor TEventAggregator.Create;
begin
  HandlerResultProcessing := procedure(Target: TObject; Result: TValue)
    begin
    end;
end;

destructor TEventAggregator.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

function TEventAggregator.HandlerExistsFor(MessageType: PTypeInfo): Boolean;
var
  LHandler: THandler;
begin
  // return handlers.Any(handler => handler.Handles(messageType) & !handler.IsDead);
  Result := False;
  TMonitor.Enter(FHandlers);
  try
    for LHandler in FHandlers do
    begin
      if LHandler.Handles(MessageType) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    TMonitor.Exit(FHandlers);
  end;
end;

procedure TEventAggregator.Publish(AMessage: TValue; Marshal: TProc<TProc>;
  AutoFree: Boolean = True);
var
  LToNotify: TArray<THandler>;
  LLifeTimeWatcher: IInterface;
begin
  Guard.CheckNotNull(AMessage, 'AMessage');

  // TODO: Translate to guard
  if not Assigned(Marshal) then
    raise EArgumentNilException.Create('Marshal');

  if AMessage.IsObject then
  begin
    Supports(AMessage.AsObject, IInterface, LLifeTimeWatcher);
  end
  else if AMessage.Kind = tkInterface then
  begin
    Supports(AMessage.AsInterface, IInterface, LLifeTimeWatcher);
  end
  else
  begin
    LLifeTimeWatcher := nil;
  end;

  TMonitor.Enter(FHandlers);
  try
    LToNotify := FHandlers.ToArray;
  finally
    TMonitor.Exit(FHandlers);
  end;

  Marshal(
    procedure
    var
      LHandler: THandler;
      LDead: IList<THandler>;
    begin
      try
        LDead := TList<THandler>.Create;
        for LHandler in LToNotify do
        begin
          if not LHandler.Handle(AMessage.TypeInfo, AMessage) then
          begin
            LDead.Add(LHandler);
          end;
        end;

        if LDead.Count > 0 then
        begin
          TMonitor.Enter(FHandlers);
          try
            for LHandler in LDead do
            begin
              FHandlers.Remove(LHandler);
            end;
          finally
            TMonitor.Exit(FHandlers);
          end;
        end;
      finally
        if AutoFree then
        begin
          if not Assigned(LLifeTimeWatcher) and AMessage.IsObject then
          begin
            AMessage.AsObject.Free;
          end;
        end;
      end;
    end);
end;

procedure TEventAggregator.PublishOnBackgroundThread(const AMessage: TValue);
begin
  Publish(AMessage, Execute.OnBackgroundThread);
end;

procedure TEventAggregator.PublishOnCurrentThread(const AMessage: TValue);
begin
  Publish(AMessage,
    procedure(Action: TProc)
    begin
      Action();
    end);
end;

procedure TEventAggregator.PublishOnUIThread(const AMessage: TValue);
begin
  Publish(AMessage, Execute.OnUIThread);
end;

procedure TEventAggregator.Subscribe(Subscriber: TObject);
var
  LHandler: THandler;
begin
  Guard.CheckNotNull(Subscriber, 'Subscriber');

  TMonitor.Enter(FHandlers);
  try
    for LHandler in FHandlers do
    begin
      if LHandler.Matches(Subscriber) then
        Exit;
    end;

    FHandlers.Add(THandler.Create(Subscriber));
  finally
    TMonitor.Exit(FHandlers);
  end;
end;

procedure TEventAggregator.Unsubscribe(Subscriber: TObject);
var
  LHandler, LFound: THandler;
begin
  Guard.CheckNotNull(Subscriber, 'Subscriber');

  TMonitor.Enter(FHandlers);
  try
    LFound := nil;
    for LHandler in FHandlers do
    begin
      if LHandler.Matches(Subscriber) then
      begin
        LFound := LHandler;
        Break;
      end;
    end;

    if Assigned(LFound) then
    begin
      FHandlers.Remove(LFound);
    end;
  finally
    TMonitor.Exit(FHandlers);
  end;
end;

{ TEventAggregator.THandler }

constructor TEventAggregator.THandler.Create(Handler: TObject);
var
  LInterface: TRttiInterfaceType;
  LType: PTypeInfo;
  LMethods: TDictionary<PTypeInfo, TRttiMethod>;
  LMethod: TRttiMethod;
begin
  FReference := Handler;
  FSupportedHandlers := TDictionary<PTypeInfo, TRttiMethod>.Create;

  LMethods := GetHandleMethods(Handler.ClassType);
  try
    for LInterface in GetRttiType(Handler.ClassInfo)
      .AsInstance.GetImplementedInterfaces do
    begin
      if TType.IsAssignable(LInterface.Handle, TypeInfo(IHandle)) and
        LInterface.IsGenericTypeDefinition then
      begin
        LType := LInterface.GetGenericArguments[0].Handle;
        LMethod := LMethods[LType];
        Log.LogMessage('Added Event Handler %s for %s.',
          [LMethod.Describe(), Handler.Describe()]);
        FSupportedHandlers.AddOrSetValue(LType, LMethod);
      end;
    end;
  finally
    LMethods.Free;
  end;
end;

destructor TEventAggregator.THandler.Destroy;
begin
  FSupportedHandlers.Free;
  inherited;
end;

function TEventAggregator.THandler.GetHandleMethods(AType: TClass)
  : TDictionary<PTypeInfo, TRttiMethod>;
var
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
begin
  Result := TDictionary<PTypeInfo, TRttiMethod>.Create;
  for LMethod in GetRttiType(AType).GetMethods do
  begin
    if (LMethod.MethodKind in [mkProcedure]) and (LMethod.Name = 'Handle') then
    begin
      LParameters := LMethod.GetParameters;
      if Length(LParameters) = 1 then
      begin
        Result.AddOrSetValue(LParameters[0].ParamType.Handle, LMethod);
      end;
    end;
  end;
end;

class function TEventAggregator.THandler.GetLog(): ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(THandler));
  end;
  Result := FLog;
end;

function TEventAggregator.THandler.Handle(MessageType: PTypeInfo;
AMessage: TValue): Boolean;
var
  LTarget: TObject;
  LPair: TPair<PTypeInfo, TRttiMethod>;
  LResult: TValue;
  LValue: TValue;
begin
  LTarget := FReference;
  if LTarget = nil then
    Exit(False);

  for LPair in FSupportedHandlers do
  begin
    if TType.IsAssignable(MessageType, LPair.Key) then
    begin
      if AMessage.TryConvert(LPair.Key, LValue) then
      begin
        LResult := LPair.Value.Invoke(LTarget, [LValue]);
        if not LResult.IsEmpty then
        begin
          HandlerResultProcessing(LTarget, LResult);
        end;
      end;
    end;
  end;

  Result := True;
end;

function TEventAggregator.THandler.Handles(MessageType: PTypeInfo): Boolean;
var
  LPair: TPair<PTypeInfo, TRttiMethod>;
begin
  Result := False;
  for LPair in FSupportedHandlers do
  begin
    if TType.IsAssignable(MessageType, LPair.Key) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TEventAggregator.THandler.Matches(Instance: TObject): Boolean;
begin
  Result := FReference = Instance;
end;

initialization

TEventAggregator.ClassName;

end.
