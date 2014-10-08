(*
  Copyright (c) 2011-2014, Stefan Glienke
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

unit DSharp.Core.Locking;

{$STACKFRAMES ON}

interface

type
  ILock = interface
    function TryEnter: Boolean;
//    procedure Enter;
    procedure Leave;
  end;

function Lock: ILock;

implementation

uses
  Classes;

type
  TLock = class(TInterfacedObject, ILock)
  private
    fAddress: Pointer;
    fLockCount: Integer;
    class var fLocks: TThreadList;
  public
    constructor Create(address: Pointer);
    destructor Destroy; override;

    class constructor Create;
    class destructor Destroy;

    function TryEnter: Boolean;
//    procedure Enter;
    procedure Leave;
  end;

{$IFDEF MSWINDOWS}
function RtlCaptureStackBackTrace(FramesToSkip: Cardinal; FramesToCapture: Cardinal;
  out BackTrace: Pointer; BackTraceHash: PCardinal): Word; stdcall; external 'kernel32';

function CallerAddr: Pointer;
begin
  // Skip 2 Frames, one for the return of CallerAddr and one for the
  // return of RtlCaptureStackBackTrace
  if RtlCaptureStackBackTrace(2, 1, Result, nil) > 0 then
    Result := Pointer(NativeInt(Result) - 5)
  else
    Result := nil;
end;
{$ELSE}
  {$IFDEF CPUX86}
function CallerAddr: Pointer;
type
  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallerFrame: PStackFrame;
    CallerAddr: PByte;
  end;
asm
  mov eax,ebp
  mov eax,[eax].TStackFrame.CallerAddr
  add eax,-5
end;
  {$ENDIF}
{$ENDIF}

function Lock: ILock;
begin
  Result := TLock.Create(CallerAddr);
end;

{ TLock }

constructor TLock.Create(address: Pointer);
begin
  inherited Create;
  fAddress := address;
end;

destructor TLock.Destroy;
begin
  if fLockCount > 1 then
    fLockCount := 1;
  Leave;
  inherited;
end;

class constructor TLock.Create;
begin
  fLocks := TThreadList.Create;
end;

class destructor TLock.Destroy;
begin
  fLocks.Free;
end;

procedure TLock.Leave;
var
  i: Integer;
  locks: TList;
begin
  if fLockCount > 0 then
  begin
    Dec(fLockCount);

    if fLockCount = 0 then
    begin
      locks := fLocks.LockList;
      try
        i := locks.IndexOf(fAddress);
        if i > -1 then
          locks.Delete(i);
      finally
        fLocks.UnlockList;
      end;
    end;
  end;
end;

function TLock.TryEnter: Boolean;
var
  locks: TList;
begin
  if fLockCount = 0 then
  begin
    locks := fLocks.LockList;
    try
      if locks.IndexOf(fAddress) = -1 then
      begin
        locks.Add(fAddress);
        Result := True;
      end
      else
        Result := False;
    finally
      fLocks.UnlockList;
    end
  end
  else
    Result := True;

  if Result then
    Inc(fLockCount);
end;

end.
