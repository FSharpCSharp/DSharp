(*
  Copyright (c) 2012, Stefan Glienke
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
  LTarget: TValue;
begin
  TValue.Make(@Instance, TypeInfo, LTarget);
  LInput := TMethodInvocation.Create(Method, LTarget, Copy(Args, 1));

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
