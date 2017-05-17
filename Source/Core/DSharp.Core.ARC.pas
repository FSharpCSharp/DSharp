(*
  Copyright (c) 2011-2013, Stefan Glienke
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

unit DSharp.Core.ARC;

{$WARN SYMBOL_DEPRECATED OFF}

interface

function __ObjAddRef(Instance: TObject): Integer;
function __ObjRelease(Instance: TObject): Integer;
function GetRefCount(Instance: TObject): Integer;

implementation

uses
  SysUtils,
  Windows;

procedure WriteMemory(const Target: Pointer; Buffer: Pointer; const Size: Cardinal);
var
  n: UINT_PTR;
begin
  if not WriteProcessMemory(GetCurrentProcess, Target, Buffer, Size, n) then
    RaiseLastOSError;
end;

function GetRefCountFieldAddress(Instance: TObject): PInteger; inline;
begin
  // the RefCount field was added last
  Result := PInteger(NativeInt(Instance) + Instance.InstanceSize);
end;

function GetRefCount(Instance: TObject): Integer; inline;
begin
  Result := GetRefCountFieldAddress(Instance)^;
end;

procedure BeforeDestruction(Self: TObject);
begin
  if GetRefCount(Self) <> 0 then
    System.Error(reInvalidPtr);
end;

function InitInstance(Self: TClass; Instance: Pointer): TObject;
const
  Buffer: Pointer = @BeforeDestruction;
begin
  Result := Self.InitInstance(Instance);

  // initialize the RefCount field
  GetRefCountFieldAddress(Instance)^ := 0;

  // replace TObject.BeforeDestruction
  if PPointer(NativeInt(Self) + vmtBeforeDestruction)^ = @TObject.BeforeDestruction then
    WriteMemory(PPointer(NativeInt(Self) + vmtBeforeDestruction), @Buffer, SizeOf(Pointer));
end;

function NewInstance(Self: TClass): TObject;
begin
  // get additional memory for the RefCount field
  GetMem(Pointer(Result), Self.InstanceSize + hfFieldSize);
  Result := InitInstance(Self, Result);
end;

function __ObjAddRef(Instance: TObject): Integer;
begin
  Result := InterlockedIncrement(GetRefCountFieldAddress(Instance)^);
end;

function __ObjRelease(Instance: TObject): Integer;
begin
  Result := InterlockedDecrement(GetRefCountFieldAddress(Instance)^);
  if Result = 0 then
    Instance.Destroy;
end;

procedure InitializeARC;
var
  Buffer: array[0..4] of Byte;
begin
  Buffer[0] := $E9;
  // redirect TObject.NewInstance
  PInteger(@Buffer[1])^ := PByte(@NewInstance) - (PByte(@TObject.NewInstance) + 5);
  WriteMemory(@TObject.NewInstance, @Buffer, 5);
end;

initialization
  InitializeARC;

end.
