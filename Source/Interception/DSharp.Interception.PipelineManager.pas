unit DSharp.Interception.PipelineManager;

interface

uses
  DSharp.Interception,
  DSharp.Interception.HandlerPipeline,
  DSharp.Interception.MethodImplementationInfo,
  Generics.Collections,
  Rtti;

type
  IPipelineManager = interface
    ['{9A368845-55BE-4D06-840E-6D9C11B4F9B1}']
    function GetPipeline(Method: TRttiMethod): THandlerPipeline;
    procedure SetPipeline(Method: TRttiMethod; Pipeline: THandlerPipeline);
    function InitializePipeline(Method: TMethodImplementationInfo; const Handlers: array of ICallHandler): Boolean;
  end;

  TPipelineManager = class(TInterfacedObject, IPipelineManager)
  private
    FPipelines: TDictionary<TRttiMethod, THandlerPipeline>;
    class var FEmptyPipeline: THandlerPipeline;
    function CreatePipeline(Method: TRttiMethod; const Handlers: array of ICallHandler): THandlerPipeline;
  public
    constructor Create;
    destructor Destroy; override;

    class constructor Create;
    class destructor Destroy;

    function GetPipeline(Method: TRttiMethod): THandlerPipeline;
    procedure SetPipeline(Method: TRttiMethod; Pipeline: THandlerPipeline);

    function InitializePipeline(Method: TMethodImplementationInfo; const Handlers: array of ICallHandler): Boolean;
  end;

implementation

{ TPipelineManager }

constructor TPipelineManager.Create;
begin
  FPipelines := TObjectDictionary<TRttiMethod, THandlerPipeline>.Create([doOwnsValues]);
end;

destructor TPipelineManager.Destroy;
begin
  FPipelines.Free;
  inherited;
end;

class constructor TPipelineManager.Create;
begin
  FEmptyPipeline := THandlerPipeline.Create;
end;

class destructor TPipelineManager.Destroy;
begin
  FEmptyPipeline.Free;
end;

function TPipelineManager.CreatePipeline(Method: TRttiMethod;
  const Handlers: array of ICallHandler): THandlerPipeline;
begin
  if not FPipelines.TryGetValue(Method, Result) then
  begin
    Result := THandlerPipeline.Create(Handlers);
  end;
end;

function TPipelineManager.GetPipeline(Method: TRttiMethod): THandlerPipeline;
begin
  if not FPipelines.TryGetValue(Method, Result) then
  begin
    Result := FEmptyPipeline;
  end;
end;

function TPipelineManager.InitializePipeline(Method: TMethodImplementationInfo;
  const Handlers: array of ICallHandler): Boolean;
var
  pipeline: THandlerPipeline;
begin
  pipeline := CreatePipeline(Method.ImplementationMethodInfo, Handlers);
  if Assigned(Method.InterfaceMethodInfo) then
  begin
    FPipelines.AddOrSetValue(Method.InterfaceMethodInfo, pipeline);
  end;

  Result := pipeline.Count > 0;
end;

procedure TPipelineManager.SetPipeline(Method: TRttiMethod;
  Pipeline: THandlerPipeline);
begin
  FPipelines.AddOrSetValue(Method, Pipeline);
end;

end.
