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

unit DSharp.Interception.HandlerPipeline;

interface

uses
  DSharp.Interception,
  Generics.Collections;

type
  THandlerPipeline = class
  private
    FHandlers: TList<ICallHandler>;
    function GetCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const Handlers: array of ICallHandler); overload;
    destructor Destroy; override;

    function Invoke(Input: IMethodInvocation; Target: TInvokeHandlerDelegate): IMethodReturn;

    property Count: Integer read GetCount;
  end;

implementation

{ THandlerPipeline }

constructor THandlerPipeline.Create;
begin
  FHandlers := TList<ICallHandler>.Create;
end;

constructor THandlerPipeline.Create(const Handlers: array of ICallHandler);
begin
  FHandlers := TList<ICallHandler>.Create;
  FHandlers.AddRange(Handlers);
end;

destructor THandlerPipeline.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

function THandlerPipeline.GetCount: Integer;
begin
  Result := FHandlers.Count;
end;

function THandlerPipeline.Invoke(Input: IMethodInvocation;
  Target: TInvokeHandlerDelegate): IMethodReturn;
var
  handlerIndex: Integer;
begin
  if FHandlers.Count = 0 then
  begin
    Result := Target(input, nil);
  end
  else
  begin
    handlerIndex := 0;

    Result := FHandlers[0].Invoke(input,
      function: TInvokeHandlerDelegate
      begin
        Inc(handlerIndex);
        if handlerIndex < FHandlers.Count then
        begin
          Result := TInvokeHandlerDelegate(FHandlers[handlerIndex]);
        end
        else
        begin
          Result := Target;
        end;
      end);
  end;
end;

end.
