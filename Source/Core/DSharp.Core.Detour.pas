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

unit DSharp.Core.Detour;

interface

type
  TXRedirCode = packed record
    Jump: Byte;
    Offset: Integer;
  end;

procedure HookCode(const Proc, Dest: Pointer; var BackupCode: TXRedirCode);
procedure PatchCode(const Proc, Dest: Pointer);
procedure UnhookCode(const Proc: Pointer; var BackupCode: TXRedirCode);
procedure WriteMem(const Location, Buffer: Pointer; const Size: Cardinal);

implementation

uses
  SysUtils,
  Windows;

resourcestring
  RMemoryWriteError = 'Error writing memory (%s)';

procedure HookCode(const Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  Code: TXRedirCode;
  n: {$IF CompilerVersion > 22}NativeUInt{$ELSE}Cardinal{$IFEND};
begin
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then
  begin
    Code.Jump := $E9;
    Code.Offset := PByte(Dest) - PByte(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure PatchCode(const Proc, Dest: Pointer);
begin
  WriteMem(Proc, @Dest, SizeOf(Pointer));
end;

procedure UnhookCode(const Proc: Pointer; var BackupCode: TXRedirCode);
var
  n: {$IF CompilerVersion > 22}NativeUInt{$ELSE}Cardinal{$IFEND};
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;

procedure WriteMem(const Location, Buffer: Pointer; const Size: Cardinal);
var
  WrittenBytes: {$IF COMPILERVERSION > 22}NativeUInt;{$ELSE}Cardinal;{$IFEND}
  SaveFlag: Cardinal;
begin
  if VirtualProtect(Location, Size, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    if not WriteProcessMemory(GetCurrentProcess, Location, Buffer, Size, WrittenBytes) then
      raise Exception.CreateResFmt(@RMemoryWriteError, [SysErrorMessage(GetLastError)]);
  finally
    VirtualProtect(Location, Size, SaveFlag, @SaveFlag);
  end;

  if WrittenBytes <> Size then
    raise Exception.CreateResFmt(@RMemoryWriteError, [IntToStr(WrittenBytes)]);

  FlushInstructionCache(GetCurrentProcess, Location, Size);
end;

end.
