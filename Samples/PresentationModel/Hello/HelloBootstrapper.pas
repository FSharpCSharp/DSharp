unit HelloBootstrapper;

interface

uses
  Interfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.Bootstrapper,
  Spring.Services,
  Spring.Container,
  TypInfo;

type
  THelloBootstrapper = class(TBootstrapper<IShellViewModel>)
  protected
    procedure Configure(); override;
    function GetAllInstances(Service: PTypeInfo): TArray<TValue>; override;
    function GetInstance(Service: PTypeInfo; Key: string): TValue; override;
  public
    constructor Create; reintroduce;
  end;

implementation

uses
  DSharp.PresentationModel.VCLWindowManager,
  zShellViewModel,
  Dialogs,
  HelloLogger;

{ THelloBootstrapper }

procedure THelloBootstrapper.Configure;
begin
  THelloLogger.Log.LogMessage('THelloBootstrapper.Configure()');
  inherited;

  // Register Shell
  // Note the framework can figure out the interfaces the class implements.
  // GlobalContainer.RegisterType<TShellViewModel>.Implements<IShellViewModel>.AsSingleton(TRefCounting.True);
  GlobalContainer.RegisterType<TShellViewModel>.AsSingleton(TRefCounting.True);

  // Register Window Manager
  // GlobalContainer.RegisterType<TWindowManager>.Implements<IWindowManager>.AsSingleton();
  GlobalContainer.RegisterType<TWindowManager>.AsSingleton();

  GlobalContainer.Build();
end;

constructor THelloBootstrapper.Create;
begin
  THelloLogger.Log.LogMessage('Boot: THelloBootstrapper.Create()');
  Showmessage('Boot');
  inherited;
end;

function THelloBootstrapper.GetAllInstances(Service: PTypeInfo): TArray<TValue>;
begin
  THelloLogger.Log.LogMessage('THelloBootstrapper.GetAllInstances(%s)',
    [Service.NameFld.ToString()]);
  Result := GlobalContainer.ResolveAll(Service);
end;

function THelloBootstrapper.GetInstance(Service: PTypeInfo;
  Key: string): TValue;
begin
  if '' = Key then
  begin
    THelloLogger.Log.LogMessage('THelloBootstrapper.GetInstance(Service=%s)',
      [Service.NameFld.ToString()]);
    Result := GlobalContainer.Resolve(Service);
  end
  else
  begin
    THelloLogger.Log.LogMessage
      ('THelloBootstrapper.GetInstance(Key=%s)', [Key]);
    Result := GlobalContainer.Resolve(Key);
  end;
end;

end.
