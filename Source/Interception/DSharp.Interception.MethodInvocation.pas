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

unit DSharp.Interception.MethodInvocation;

interface

uses
  DSharp.Interception,
  Rtti,
  SysUtils;

type
  TMethodInvocation = class(TInterfacedObject, IMethodInvocation)
  private
    FArguments: TArray<TValue>;
    FMethod: TRttiMethod;
    FTarget: TValue;
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetTarget: TValue;
  public
    constructor Create(Method: TRttiMethod; Target: TValue; const Arguments: TArray<TValue>);

    function CreateMethodReturn(const ReturnValue: TValue): IMethodReturn;
    function CreateExceptionMethodReturn(E: Exception): IMethodReturn;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Target: TValue read GetTarget;
  end;

implementation

uses
  DSharp.Interception.MethodReturn;

{ TMethodInvocation }

constructor TMethodInvocation.Create(Method: TRttiMethod; Target: TValue;
  const Arguments: TArray<TValue>);
begin
  FMethod := Method;
  FTarget := Target;
  FArguments := Arguments;
end;

function TMethodInvocation.CreateExceptionMethodReturn(
  E: Exception): IMethodReturn;
begin
  Result := TMethodReturn.Create(FMethod);
  Result.Exception := E;
end;

function TMethodInvocation.CreateMethodReturn(
  const ReturnValue: TValue): IMethodReturn;
begin
  Result := TMethodReturn.Create(FMethod);
  Result.ReturnValue := ReturnValue;
end;

function TMethodInvocation.GetArguments: TArray<TValue>;
begin
  Result := FArguments;
end;

function TMethodInvocation.GetMethod: TRttiMethod;
begin
  Result := FMethod;
end;

function TMethodInvocation.GetTarget: TValue;
begin
  Result := FTarget;
end;

end.
