unit AppBootstrapper;

interface

uses
  Classes,
  SysUtils,
  DSharp.PresentationModel,
  DSharp.PresentationModel.Bootstrapper,
  TypInfo,
  zShellViewModel,
  DSharp.Core.Collections,
  Spring.Container,
  Spring.Services,
  Interfaces;

type
  ApplicationBootstrapper = class(TBootstrapper<IShell>)
  protected
    procedure BuildUp(AInstance: TObject); override;
    procedure Configure; override;
    function GetAllInstances(Service: PTypeInfo): TArray<TValue>; override;
    function GetInstance(Service: PTypeInfo; Key: string): TValue; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartRuntime; override;
  end;

implementation

uses
  Registry,
  Windows,
  Dialogs,
  Forms,
  DSharp.Logging,
  DSharp.PresentationModel.VCLWindowManager,
  DSharp.PresentationModel.EventAggregator,
  DSharp.Logging.Debug;

constructor ApplicationBootstrapper.Create;
begin
  inherited Create();
  // Bring application to front
  SetForegroundWindow(Application.Handle);
end;

destructor ApplicationBootstrapper.Destroy;
begin
  // Call base destructor
  inherited;
end;

procedure ApplicationBootstrapper.BuildUp(AInstance: TObject);
begin
  raise ENotImplemented.Create('BuildUp');
end;

{ ApplicationBootstrapper }

procedure ApplicationBootstrapper.Configure;
begin
  inherited;

  // Register Shell
  GlobalContainer.RegisterType<TShellViewModel>.Implements<IShell>.AsSingleton
    (TRefCounting.True);

  // Register Event Aggregator
  GlobalContainer.RegisterType<TEventAggregator>.
    Implements<IEventAggregator>.AsSingleton;

  // Register Window Manager
  GlobalContainer.RegisterType<TWindowManager>.
    Implements<IWindowManager>.AsSingleton;

  GlobalContainer.Build;
end;

function ApplicationBootstrapper.GetAllInstances(Service: PTypeInfo)
  : TArray<TValue>;
begin
  Result := GlobalContainer.ResolveAll(Service);
end;

function ApplicationBootstrapper.GetInstance(Service: PTypeInfo;
  Key: string): TValue;
begin
  if Length(Key) = 0 then
    Result := GlobalContainer.Resolve(Service)
  else
    Result := GlobalContainer.Resolve(Key);
end;

procedure ApplicationBootstrapper.StartRuntime;
begin
  LogManager.GetLog := function(TypeInfo: PTypeInfo): ILog
    begin
      Result := TDebugLog.Create;
    end;

  inherited;
end;

end.
