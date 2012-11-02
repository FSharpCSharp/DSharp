unit DSharp.Interception.InterfaceProxy;

interface

uses
  DSharp.Interception,
  DSharp.Interception.InterceptionBehaviorPipeline,
  Rtti,
  DSharp.Core.VirtualInterface,
  TypInfo;

type
  TInterfaceProxy = class(TVirtualInterface, IInterceptingProxy)
  private
    FInterceptionBehaviorPipeline: TInterceptionBehaviorPipeline;
    procedure AddInterceptionBehavior(Interceptor: IInterceptionBehavior);
  protected
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(TypeInfo: PTypeInfo; Target: Pointer);
    destructor Destroy; override;
  end;

implementation

uses
  DSharp.Interception.MethodInvocation,
  SysUtils;

{ TInterfaceProxy }

constructor TInterfaceProxy.Create(TypeInfo: PTypeInfo; Target: Pointer);
begin
  inherited Create(TypeInfo, Invoke);
  FInterceptionBehaviorPipeline := TInterceptionBehaviorPipeline.Create;
  Instance := IInterface(Target);
end;

destructor TInterfaceProxy.Destroy;
begin
  FInterceptionBehaviorPipeline.Free;
  inherited;
end;

procedure TInterfaceProxy.AddInterceptionBehavior(
  Interceptor: IInterceptionBehavior);
begin
  FInterceptionBehaviorPipeline.Add(Interceptor);
end;

procedure TInterfaceProxy.Invoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  i: Integer;
  LInput: IMethodInvocation;
  LReturn: IMethodReturn;
begin
  LInput := TMethodInvocation.Create(Method, TValue.From<IInterface>(Instance), Copy(Args, 1));

  LReturn := FInterceptionBehaviorPipeline.Invoke(LInput,
    function(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn
    var
      LArgs: TArray<TValue>;
    begin
      LArgs := Input.Arguments;
      if Assigned(Instance) then
      begin
        try
          Result := Input.CreateMethodReturn(
            Method.Invoke(TValue.From<IInterface>(Instance), LArgs));
        except
          Result := Input.CreateExceptionMethodReturn(AcquireExceptionObject);
        end;
      end;
    end);

  if Assigned(LReturn.Exception) then
  begin
    raise LReturn.Exception;
  end;
  Result := LReturn.ReturnValue;

  for i := 1 to High(Args) do
  begin
    Args[i] := LInput.Arguments[i - 1];
  end;
end;

function TInterfaceProxy.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
  begin
    Result := 0
  end
  else
  begin
    Result := inherited;
  end;
end;

end.
