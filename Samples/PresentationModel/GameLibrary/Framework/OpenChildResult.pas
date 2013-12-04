unit OpenChildResult;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  OpenResultBase;

type
  TOpenChildResult<TChild> = class(TOpenResultBase<TChild>)
  private
    FChild: TChild;
    FParent: IConductor;
    FProcessed: TActivationProcessedEvent;
    LocateChild: TFunc<IActionExecutionContext, TChild>;
    LocateParent: TFunc<IActionExecutionContext, IConductor>;
  protected
    procedure OnOpened(Parent: IConductor; Child: TChild); virtual;
  public
    constructor Create;
    function Inside<TParent>: TOpenChildResult<TChild>; overload;
    // function Inside(Parent: IConductor): TOpenChildResult<TChild>; overload;
    procedure Execute(Context: IActionExecutionContext); override;
    procedure Processed(Sender: TObject; Args: IActivationProcessedEventArgs);
  end;

implementation

uses
  DSharp.Core.Reflection,
  TypInfo,
  DSharp.Core.EventArgs;

constructor TOpenChildResult<TChild>.Create;
begin
  LocateChild := function(Context: IActionExecutionContext): TChild
    begin
      Result := IoC.Get<TChild>();
    end;
  LocateParent := function(Context: IActionExecutionContext): IConductor
    begin
      // Result := Context.Target;
    end;

  FProcessed := Processed;
end;

procedure TOpenChildResult<TChild>.Execute(Context: IActionExecutionContext);
var
  LObject: TObject;
begin
  FParent := LocateParent(Context);
  FChild := LocateChild(Context);

  if Assigned(OnConfigure) then
    OnConfigure(FChild);

  if PTypeInfo(TypeInfo(TChild)).Kind = tkInterface then
    LObject := TValue.From<TChild>(FChild).AsInterface as TObject
  else
    LObject := TValue.From<TChild>(FChild).AsObject;

  FParent.ActivationProcessed.Add(FProcessed);
  FParent.ActivateItem(LObject);
end;

{
  function TOpenChildResult<TChild>.Inside(Parent: IConductor): TOpenChildResult<TChild>;
  begin
  LocateParent := function(Context: IActionExecutionContext): IConductor
  begin
  Result := Parent;
  end;
  Result := Self;
  end;
}

function TOpenChildResult<TChild>.Inside<TParent>: TOpenChildResult<TChild>;
begin
  LocateParent := function(Context: IActionExecutionContext): IConductor
    begin
      Supports(TValue.From<TParent>(IoC.Get<TParent>()), IConductor, Result);
    end;
  Result := Self;
end;

procedure TOpenChildResult<TChild>.OnOpened(Parent: IConductor; Child: TChild);
begin
end;

procedure TOpenChildResult<TChild>.Processed(Sender: TObject;
  Args: IActivationProcessedEventArgs);
var
  LDeactivator: IDeactivate;
  LHandler: TDeactivationEvent;
begin
  FParent.ActivationProcessed.Remove(FProcessed);

  if Args.Success then
  begin
    OnOpened(FParent, FChild);

    if Supports(TValue.From<TChild>(FChild), IDeactivate, LDeactivator) and
      Assigned(OnClose) then
    begin
      LHandler :=
          procedure(Sender2: TObject; EventArgs2: IDeactivationEventArgs)
        begin
          if not EventArgs2.WasClosed then
            Exit;

          LDeactivator.Deactivated.Remove(LHandler);
          OnClose(FChild);
        end;

      LDeactivator.Deactivated.Add(LHandler);
    end;

    Completed.Invoke(Self, TResultCompletionEventArgs.Create);
  end
  else
  begin
    Completed.Invoke(Self, TResultCompletionEventArgs.Create(nil, True));
  end;
end;

end.
