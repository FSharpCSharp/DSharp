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

unit DSharp.Interception.Logging;

interface

uses
  DSharp.Interception,
  DSharp.Interception.CallHandler,
  Rtti,
  SysUtils;

type
  LoggingCallHandlerAttribute = class(HandlerAttribute)
  public
    function CreateHandler: ICallHandler; override;
  end;

  TLoggingCallHandler = class(TCallHandler)
  public
    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeHandlerDelegate>): IMethodReturn; override;
  end;

  TLoggingBehavior = class(TInterfacedObject, IInterceptionBehavior)
  public
    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
    function WillExecute: Boolean;
  end;

implementation

uses
  DSharp.Logging;

procedure EnterMethod(Input: IMethodInvocation);
var
  params: TArray<TRttiParameter>;
  i: Integer;
begin
  Logging.EnterMethod(Input.Target, Input.Method.Name);

  params := Input.Method.GetParameters;
  for i := Low(params) to High(params) do
  begin
    Logging.LogValue(params[i].Name, Input.Arguments[i]);
  end;
end;

procedure LeaveMethod(Input: IMethodInvocation; Result: IMethodReturn);
begin
  if Assigned(Input.Method.ReturnType) then
  begin
    Logging.LogValue('Result', Result.ReturnValue);
  end;

  Logging.LeaveMethod(Input.Target, Input.Method.Name);
end;

{ LoggingCallHandlerAttribute }

function LoggingCallHandlerAttribute.CreateHandler: ICallHandler;
begin
  Result := TLoggingCallHandler.Create;
end;

{ TLoggingCallHandler }

function TLoggingCallHandler.Invoke(Input: IMethodInvocation;
  GetNext: TFunc<TInvokeHandlerDelegate>): IMethodReturn;
begin
  EnterMethod(Input);

  Result := GetNext().Invoke(Input, GetNext);

  LeaveMethod(Input, Result);
end;

{ TLoggingBehavior }

function TLoggingBehavior.Invoke(Input: IMethodInvocation;
  GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
begin
  EnterMethod(Input);

  Result := GetNext().Invoke(Input, GetNext);

  LeaveMethod(Input, Result);
end;

function TLoggingBehavior.WillExecute: Boolean;
begin
  Result := True;
end;

end.
