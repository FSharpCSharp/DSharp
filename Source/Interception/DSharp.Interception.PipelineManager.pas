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
