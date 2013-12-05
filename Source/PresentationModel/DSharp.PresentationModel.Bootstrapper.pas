unit DSharp.PresentationModel.Bootstrapper;

interface

uses
  SysUtils,
  TypInfo,
  DSharp.ComponentModel.Composition.SpringContainer,
  DSharp.Collections,
  DSharp.PresentationModel;

type
  /// <summary>
  /// Instantiate this class in order to configure the framework.
  /// </summary>
  TBootstrapper = class abstract
  protected
    /// <summary>
    /// Provides an opportunity to hook into the application object.
    /// </summary>
    procedure PrepareApplication; virtual;

    /// <summary>
    /// Override to configure the framework and setup your IoC container.
    /// </summary>
    procedure Configure; virtual;

    /// <summary>
    /// Override this to provide an IoC specific implementation.
    /// </summary>
    /// <param name="service">
    /// The service to locate.
    /// </param>
    /// <param name="key">
    /// The key to locate.
    /// </param>
    /// <returns>
    /// The located service.
    /// </returns>
    function GetInstance(Service: PTypeInfo; Key: string): TValue; virtual;

    /// <summary>
    /// Override this to provide an IoC specific implementation
    /// </summary>
    /// <param name="service">
    /// The service to locate.
    /// </param>
    /// <returns>
    /// The located services.
    /// </returns>
    function GetAllInstances(Service: PTypeInfo): TArray<TValue>; virtual;

    /// <summary>
    /// Override this to provide an IoC specific implementation.
    /// </summary>
    /// <param name="instance">
    /// The instance to perform injection on.
    /// </param>
    procedure BuildUp(Instance: TObject); virtual;

    /// <summary>
    /// Override this to add custom behavior to execute after the application
    /// starts.
    /// </summary>
    /// <param name="sender">
    /// The sender.
    /// </param>
    /// <param name="e">
    /// The args.
    /// </param>
    procedure OnStartup(Sender: TObject); virtual;

    /// <summary>
    /// Override this to add custom behavior on exit.
    /// </summary>
    /// <param name="sender">
    /// The sender.
    /// </param>
    /// <param name="e">
    /// The event args.
    /// </param>
    procedure OnExit(Sender: TObject); virtual;

    /// <summary>
    /// Override this to add custom behavior for unhandled exceptions.
    /// </summary>
    /// <param name="sender">
    /// The sender.
    /// </param>
    /// <param name="e">
    /// The event args.
    /// </param>
    procedure OnUnhandledException(Sender: TObject; E: Exception); virtual;

    /// <summary>
    /// Locates the view model, locates the associate view, binds them and
    /// shows it as the root view.
    /// </summary>
    /// <param name="viewModelType">
    /// The view model type.
    /// </param>
    procedure DisplayRootViewFor(ViewModelType: PTypeInfo);
  public
    /// <summary>
    /// Called by Application helper at runtime to start the framework.
    /// </summary>
    procedure StartRuntime; virtual;
  end;

  TBootstrapperClass = class of TBootstrapper;

  /// <summary>
  /// A strongly-typed version of <see cref="Bootstrapper" /> that specifies
  /// the type of root model to create for the application.
  /// </summary>
  /// <typeparam name="TRootModel">
  /// The type of root model for the application.
  /// </typeparam>
  TBootstrapper<TRootModel> = class(TBootstrapper)
  protected
    /// <summary>
    /// Override this to add custom behavior to execute after the application
    /// starts.
    /// </summary>
    /// <param name="Sender">
    /// The sender.
    /// </param>
    procedure OnStartup(Sender: TObject); override;
  public
    /// <summary>
    /// Creates an instance of the bootstrapper
    /// </summary>
    constructor Create;
  end;

  TSpringBootstrapper<TRootModel> = class(TBootstrapper<TRootModel>)
  private
    FContainer: TSpringContainer;
  protected
    procedure Configure; override;
    function GetAllInstances(Service: PTypeInfo): TArray<TValue>; override;
    function GetInstance(Service: PTypeInfo; Key: string): TValue; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  // DSharp.Aspects.Weaver,
  DSharp.PresentationModel.EventAggregator,
  Spring.Reflection,
  DSharp.PresentationModel.CoroutineExecutionContext,
  DSharp.PresentationModel.CoroutineExecutionContextIntf,
  DSharp.PresentationModel.WindowManagerIntf;

procedure TBootstrapper.BuildUp(Instance: TObject);
begin
end;

procedure TBootstrapper.Configure;
begin
end;

procedure TBootstrapper.DisplayRootViewFor(ViewModelType: PTypeInfo);
var
  LWindowManager: IWindowManager;
begin
  LWindowManager := IoC.Get<IWindowManager>();

  if PTypeInfo(ViewModelType).Kind = tkClass then
  begin
    LWindowManager.ShowWindow(IoC.GetInstance(ViewModelType, '').AsObject);
  end
  else
  begin
    LWindowManager.ShowWindow(IoC.GetInstance(ViewModelType, '').AsInterface);
  end;
end;

function TBootstrapper.GetAllInstances(Service: PTypeInfo): TArray<TValue>;
begin
  SetLength(Result, 1);
  Result[0] := TActivator.CreateInstance(Service);
end;

function TBootstrapper.GetInstance(Service: PTypeInfo; Key: string): TValue;
begin
  // TODO: Should we bring in default platform TWindowManager?
  // if (Service = TypeInfo(IWindowManager)) then
  // Service:= TypeInfo(TWindowManager);
  Result := TActivator.CreateInstance(Service);
end;

procedure TBootstrapper.OnExit(Sender: TObject);
begin
end;

procedure TBootstrapper.OnStartup(Sender: TObject);
begin
end;

procedure TBootstrapper.OnUnhandledException(Sender: TObject; E: Exception);
begin
end;

procedure TBootstrapper.PrepareApplication;
begin
  // TODO: We should bring in default platform Application to wire up OnUnhandledException
  // Application.OnException := OnUnhandledException;
end;

procedure TBootstrapper.StartRuntime;
begin
  Execute.InitializeWithDispatcher();

  TEventAggregator.HandlerResultProcessing :=
      procedure(Target: TObject; Result: TValue)
    var
      LCoroutine: IEnumerable<IResult>;
      LViewAware: IViewAware;
      LView: TObject;
      LContext: ICoroutineExecutionContext;
      LObject: TObject;
    begin
      // TODO: Verify this code if converting to IEnumerable<IResult> works as expected?

      // Objective: Cast Result into IEnumerable<IResult>
      LObject := Result.AsObject;
      LCoroutine := TValue.From<TObject>(LObject).AsType<IEnumerable<IResult>>;

      if Assigned(LCoroutine) then
      begin
        if Supports(Target, IViewAware, LViewAware) then
          LView := LViewAware.GetView
        else
          LView := nil;

        LContext := TCoroutineExecutionContext.Create;
        LContext.Target := Target;
        LContext.View := LView as TComponent;

        TCoroutine.BeginExecute(LCoroutine.GetEnumerator, LContext);
      end;
    end;

  PrepareApplication();

  Configure();

  IoC.GetInstance := GetInstance;
  IoC.GetAllInstances := GetAllInstances;
  IoC.BuildUp := BuildUp;

  try
    OnStartup(Self);
  finally
    OnExit(Self);
  end;
end;

{ TBootstrapper<TRootModel> }

constructor TBootstrapper<TRootModel>.Create;
begin
  inherited Create;
  if not(PTypeInfo(TypeInfo(TRootModel)).Kind in [tkClass, tkInterface]) then
  begin
    raise Exception.CreateFmt('"%s" is not a valid class or interface type',
      [PTypeInfo(TypeInfo(TRootModel)).Name]);
  end;
end;

procedure TBootstrapper<TRootModel>.OnStartup(Sender: TObject);
begin
  DisplayRootViewFor(TypeInfo(TRootModel));
end;

{ TSpringBootstrapper<TRootModel> }

procedure TSpringBootstrapper<TRootModel>.Configure;
begin
  FContainer := TSpringContainer.Create();
  // FContainer.AspectWeaver := TAspectWeaver.Create();
  FContainer.ImportRtti();
end;

destructor TSpringBootstrapper<TRootModel>.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TSpringBootstrapper<TRootModel>.GetAllInstances(Service: PTypeInfo)
  : TArray<TValue>;
begin
  Result := FContainer.ResolveAll(Service);
end;

function TSpringBootstrapper<TRootModel>.GetInstance(Service: PTypeInfo;
  Key: string): TValue;
begin
  Result := FContainer.Resolve(Service, Key);
end;

end.
