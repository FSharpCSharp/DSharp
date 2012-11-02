unit DSharp.Interception.PolicyInjectionBehavior;

interface

uses
  DSharp.Interception,
  DSharp.Interception.HandlerPipeline,
  DSharp.Interception.PipelineManager,
  Rtti,
  SysUtils,
  TypInfo;

type
  TPolicyInjectionBehavior = class(TInterfacedObject, IInterceptionBehavior)
  private
    FPipelineManager: IPipelineManager;
    function GetPipeline(Method: TRttiMethod): THandlerPipeline;
  public
    constructor Create(PipelineManager: IPipelineManager); overload;
    constructor Create(const InterceptionRequest: TCurrentInterceptionRequest;
      const Policies: array of IInjectionPolicy); overload;

    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
    function GetRequiredInterfaces: TArray<PTypeInfo>;
    function WillExecute: Boolean;
  end;

implementation

uses
  DSharp.Interception.MethodImplementationInfo,
  DSharp.Interception.PolicySet;

{ TPolicyInjectionBehavior }

constructor TPolicyInjectionBehavior.Create(PipelineManager: IPipelineManager);
begin
  FPipelineManager := PipelineManager;
end;

constructor TPolicyInjectionBehavior.Create(
  const InterceptionRequest: TCurrentInterceptionRequest;
  const Policies: array of IInjectionPolicy);
var
  allPolicies: TPolicySet;
  hasHandlers: Boolean;
  manager: IPipelineManager;
  method: TMethodImplementationInfo;
  hasNewHandlers: Boolean;
begin
  allPolicies := TPolicySet.Create(Policies);
  hasHandlers := False;

  manager := TPipelineManager.Create;

  for method in InterceptionRequest.Interceptor.GetInterceptableMethods(
    InterceptionRequest.TypeToIntercept, InterceptionRequest.ImplementationType) do
  begin
    hasNewHandlers := manager.InitializePipeline(method,
      allPolicies.GetHandlersFor(method));
    hasHandlers := hasHandlers or hasNewHandlers;
  end;

  if hasHandlers then
  begin
    FPipelineManager := manager;
  end;

  allPolicies.Free;
end;

function TPolicyInjectionBehavior.GetPipeline(
  Method: TRttiMethod): THandlerPipeline;
begin
  Result := FPipelineManager.GetPipeline(Method);
end;

function TPolicyInjectionBehavior.GetRequiredInterfaces: TArray<PTypeInfo>;
begin
  Result := nil;
end;

function TPolicyInjectionBehavior.Invoke(Input: IMethodInvocation;
  GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
var
  pipeline: THandlerPipeline;
begin
  pipeline := GetPipeline(Input.Method);

  Result := pipeline.Invoke(Input,
    function(PolicyInjectionInput: IMethodInvocation;
      PolicyInjectionInputGetNext: TFunc<TInvokeHandlerDelegate>): IMethodReturn
    begin
      try
        Result := GetNext()(PolicyInjectionInput, GetNext);
      except
        on E: Exception do
        begin
          Result := PolicyInjectionInput.CreateExceptionMethodReturn(E.InnerException);
        end;
      end;
    end);
end;

function TPolicyInjectionBehavior.WillExecute: Boolean;
begin
  Result := Assigned(FPipelineManager);
end;

end.
