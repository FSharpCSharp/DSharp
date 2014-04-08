(*
  Copyright (c) 2012-2014, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

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
      end
      else
      begin
        Result := Input.CreateMethodReturn(TValue.Empty);
      end;
    end);

  if Assigned(LReturn) then
  begin
    if Assigned(LReturn.Exception) then
    begin
      raise LReturn.Exception;
    end;
    Result := LReturn.ReturnValue;
  end
  else
  begin
    Result := TValue.Empty;
  end;

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
