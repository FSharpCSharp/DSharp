unit DSharp.PresentationModel.SpringBootstrapper;

interface

uses
  Rtti,
  TypInfo,
  DSharp.PresentationModel.Bootstrapper,
  DSharp.ComponentModel.Composition.SpringContainer;

type
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
