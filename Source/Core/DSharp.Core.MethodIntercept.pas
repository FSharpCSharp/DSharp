(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Core.MethodIntercept;

interface

uses
  Generics.Collections,
  Rtti;

type
  TMethodInvokeEvent = reference to procedure(Method: TRttiMethod;
    const Args: TArray<TValue>; out Result: TValue);

  TMethodIntercept = class
  private
    FImplementation: TMethodImplementation;
    FMethod: TRttiMethod;
    function GetCodeAddress: Pointer;
    function GetVirtualIndex: SmallInt;
  public
    constructor Create(const Method: TRttiMethod;
      const Callback: TMethodImplementationCallback);
    destructor Destroy; override;
    property CodeAddress: Pointer read GetCodeAddress;
    property Method: TRttiMethod read FMethod;
    property VirtualIndex: SmallInt read GetVirtualIndex;
  end;

  TMethodIntercepts = TObjectList<TMethodIntercept>;

implementation

{ TMethodIntercept }

constructor TMethodIntercept.Create(const Method: TRttiMethod;
  const Callback: TMethodImplementationCallback);
begin
  FImplementation := Method.CreateImplementation(Self, Callback);
  FMethod := Method;
end;

destructor TMethodIntercept.Destroy;
begin
  FImplementation.Free;
  inherited;
end;

function TMethodIntercept.GetCodeAddress: Pointer;
begin
  Result := FImplementation.CodeAddress;
end;

function TMethodIntercept.GetVirtualIndex: SmallInt;
begin
  Result := FMethod.VirtualIndex;
end;

end.
