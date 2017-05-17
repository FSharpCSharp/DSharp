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
