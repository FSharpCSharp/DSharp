unit DSharp.Interception.ClassProxy;

interface

uses
  DSharp.Core.VirtualClass,
  DSharp.Interception,
  DSharp.Interception.InterceptionBehaviorPipeline,
  Rtti,
  TypInfo;

type
  TClassProxy = class(TVirtualClass, IInterceptingProxy)
  private
    FInterceptionBehaviorPipeline: TInterceptionBehaviorPipeline;
    FRefCount: Integer;
    procedure AddInterceptionBehavior(Interceptor: IInterceptionBehavior);
  protected
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(TypeInfo: PTypeInfo; Target: Pointer);
    destructor Destroy; override;
  end;

implementation

uses
  DSharp.Interception.MethodInvocation,
  SysUtils;

{$IF CompilerVersion < 23}
  {$DEFINE CPUX86}
{$IFEND}

{ TClassProxy }

constructor TClassProxy.Create(TypeInfo: PTypeInfo; Target: Pointer);
begin
  inherited Create(GetTypeData(TypeInfo).ClassType);
  FInterceptionBehaviorPipeline := TInterceptionBehaviorPipeline.Create;
  Instance := Target;
  OnInvoke := Invoke;
  if Assigned(Instance) then
  begin
    PPointer(Instance)^ := MetaclassType;
    FreeOnInstanceDestroy := True;
  end;
end;

destructor TClassProxy.Destroy;
begin
  FInterceptionBehaviorPipeline.Free;
  inherited;
end;

procedure TClassProxy.AddInterceptionBehavior(
  Interceptor: IInterceptionBehavior);
begin
  FInterceptionBehaviorPipeline.Add(Interceptor);
end;

procedure TClassProxy.Invoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  i: Integer;
  LInput: IMethodInvocation;
  LReturn: IMethodReturn;
begin
  LInput := TMethodInvocation.Create(Method, Instance, Copy(Args, 1));

  LReturn := FInterceptionBehaviorPipeline.Invoke(LInput,
    function(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn
    var
      LArgs: TArray<TValue>;

      procedure PassArgs;
      var
        k: Integer;
        LParams: TArray<TRttiParameter>;
      begin
        LParams := LInput.Method.GetParameters;
        SetLength(LArgs, Length(LParams) + 1);
        LArgs[0] := LInput.Target;
        for k := 1 to High(LArgs) do
        begin
{$WARNINGS OFF}
          if ((pfConst in LParams[k - 1].Flags) and (LParams[k - 1].ParamType.TypeSize > SizeOf(Pointer)))
{$IFDEF CPUX86}
            or ((Input.Method.CallingConvention in [ccCdecl, ccStdCall, ccSafeCall])
            and (pfConst in LParams[k - 1].Flags) and (LParams[k - 1].ParamType.TypeKind = tkVariant))
{$ENDIF CPUX86}
            or ([pfVar, pfOut] * LParams[k - 1].Flags <> []) then
            LArgs[k] := Input.Arguments[k - 1].GetReferenceToRawData
          else
            LArgs[k] := Input.Arguments[k - 1];
        end;
      end;

    begin
      if Assigned(Instance) then
      begin
        PassArgs;
        try
          if Input.Method.ReturnType <> nil then
          begin
            Result := Input.CreateMethodReturn(
              Rtti.Invoke(Input.Method.CodeAddress, LArgs,
              Input.Method.CallingConvention, Input.Method.ReturnType.Handle));
          end
          else
          begin
            Result := Input.CreateMethodReturn(
              Rtti.Invoke(Input.Method.CodeAddress, LArgs,
              Input.Method.CallingConvention, nil));
          end;
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

function TClassProxy.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TClassProxy._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TClassProxy._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if not FreeOnInstanceDestroy and (Result = 0) then
    Destroy;
end;

end.
