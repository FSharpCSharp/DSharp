(*
  Copyright (c) 2013, Stefan Glienke
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

unit InterfaceRttiPatch;

interface

uses
  TypInfo;

procedure PatchInterfaceRtti(ATypeInfo: PTypeInfo);

implementation

uses
  Windows;

function SkipShortString(P: Pointer): Pointer;
begin
  Result := PByte(P) + PByte(P)^ + 1;
end;

function SkipAttributes(P: Pointer): Pointer;
begin
  Result := PByte(P) + PWord(P)^;
end;

procedure PatchInterfaceRtti(ATypeInfo: PTypeInfo);
var
  typeData: PTypeData;
  table: PIntfMethodTable;
  p: PByte;
  entry: PIntfMethodEntry;
  tail: PIntfMethodEntryTail;
  methodIndex: Integer;
  paramIndex: Integer;
  next: PByte;
  n: UINT_PTR;
  count: Integer;
  doPatch: Boolean;

  function IsBrokenMethodEntry(entry: Pointer): Boolean;
  var
    p: PByte;
    tail: PIntfMethodEntryTail;
  begin
    p := entry;
    p := SkipShortString(p);
    tail := PIntfMethodEntryTail(p);
    // if ParamCount is 0 the compiler has generated
    // wrong typeinfo for a property returning a method type
    if tail.ParamCount = 0 then
      Exit(True)
    else
    begin
      Inc(p, SizeOf(TIntfMethodEntryTail));
      Inc(p, SizeOf(TParamFlags));
      // if Params[0].ParamName is not 'Self'
      // and Params[0].Tail.ParamType is not the same typeinfo as the interface
      // it is very likely that the compiler has generated
      // wrong type info for a property returning a method type
      if PShortString(p)^ <> 'Self' then
      begin
        p := SkipShortString(p); // ParamName
        p := SkipShortString(p); // TypeName
        if PIntfMethodParamTail(p).ParamType^ <> ATypeInfo then
          Exit(True);
      end;
    end;
    Result := False;
  end;

begin
  if ATypeInfo.Kind <> tkInterface then Exit;

  typeData := GetTypeData(ATypeInfo);
  table := SkipShortString(@typeData.IntfUnit);
  if table.RttiCount = $FFFF then Exit;

  next := nil;
  for doPatch in [False, True] do
  begin
    p := PByte(table);
    Inc(p, SizeOf(TIntfMethodTable));
    for methodIndex := 0 to table.Count - 1 do
    begin
      entry := PIntfMethodEntry(p);
      p := SkipShortString(p);
      tail := PIntfMethodEntryTail(p);
      Inc(p, SizeOf(TIntfMethodEntryTail));
      for paramIndex := 0 to tail.ParamCount - 1 do
      begin
        Inc(p, SizeOf(TParamFlags));  // TIntfMethodParam.Flags
        p := SkipShortString(p);      // TIntfMethodParam.ParamName
        p := SkipShortString(p);      // TIntfMethodParam.TypeName
        Inc(p, SizeOf(PPTypeInfo));   // TIntfMethodParamTail.ParamType
        p := SkipAttributes(p);       // TIntfMethodParamTail.AttrData
      end;
      if tail.Kind = 1 then // function
      begin
        p := SkipShortString(p);      // TIntfMethodEntryTail.ResultTypeName
        Inc(p, SizeOf(PPTypeInfo));   // TIntfMethodEntryTail.ResultType
      end;
      p := SkipAttributes(p);         // TIntfMethodEntryTail.AttrData

      if doPatch and IsBrokenMethodEntry(entry) then
      begin
        WriteProcessMemory(GetCurrentProcess, entry, p, next - p, n);
        count := table.Count - 1;
        p := @table.Count;
        WriteProcessMemory(GetCurrentProcess, p, @count, SizeOf(Word), n);
        count := table.RttiCount;
        p := @table.RttiCount;
        WriteProcessMemory(GetCurrentProcess, p, @count, SizeOf(Word), n);
        p := PByte(entry);
      end;
    end;
    p := SkipAttributes(p);           // TIntfMethodTable.AttrData
    next := p;
  end;
end;

end.
