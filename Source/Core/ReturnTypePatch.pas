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

unit ReturnTypePatch;

interface

uses
  Rtti,
  TypInfo;

type
  ReturnTypePatchAttribute = class(TCustomAttribute)
  private
    FReturnType: PTypeInfo;
  public
    constructor Create(ATypeInfo: PTypeInfo);
  end;

procedure PatchMethodReturnType(ATypeInfo: PTypeInfo); overload;
procedure PatchMethodReturnType(AMethod: TRttiMethod; AReturnType: PTypeInfo); overload;

implementation

uses
  Windows;

type
  TRttiIntfMethod = class(TRttiMethod)
  public
    FTail: PIntfMethodEntryTail;
    FParameters: TArray<TRttiParameter>;
    FReturnType: PTypeInfo;
  end;

var
  ReturnTypes: array of PPTypeInfo;

procedure Finalize;
var
  i: Integer;
begin
  for i := High(ReturnTypes) downto Low(ReturnTypes) do
    Dispose(ReturnTypes[i]);
end;

function NeedsPatch(AMethod: TRttiMethod): Boolean;
begin
  Result := (AMethod.MethodKind = mkFunction) and (AMethod.ReturnType = nil);
end;

procedure PatchMethodReturnType(ATypeInfo: PTypeInfo);
var
  LContext: TRttiContext;
  LMethod: TRttiMethod;
  LAttribute: TCustomAttribute;
begin
  for LMethod in LContext.GetType(ATypeInfo).GetDeclaredMethods do
  begin
    if NeedsPatch(LMethod) then
    begin
      for LAttribute in LMethod.GetAttributes do
      begin
        if LAttribute is ReturnTypePatchAttribute then
          PatchMethodReturnType(LMethod, ReturnTypePatchAttribute(LAttribute).FReturnType);
      end;
    end;
  end;
  LContext.Free;
end;

procedure PatchMethodReturnType(AMethod: TRttiMethod; AReturnType: PTypeInfo);
var
  p: PByte;
  i: Integer;
  LByteCount: Cardinal;
  LReturnType: PPTypeInfo;

  procedure SkipShortString(var p: PByte);
  begin
    Inc(p, p[0] + 1);
  end;

begin
  if not NeedsPatch(AMethod) then
    Exit;

  Pointer(p) := TRttiIntfMethod(AMethod).FTail;
  Inc(p, SizeOf(TIntfMethodEntryTail));

  for i := 0 to TRttiIntfMethod(AMethod).FTail.ParamCount - 1 do
  begin
    Inc(p);                     // Flags
    SkipShortString(p);         // ParamName
    SkipShortString(p);         // TypeName
    Inc(p, SizeOf(PTypeInfo));  // ParamType
    Inc(p, PWord(p)^);          // AttrData
  end;

  LReturnType := nil;

  for i := Low(ReturnTypes) to High(ReturnTypes) do
  begin
    if ReturnTypes[i]^ = AReturnType then
    begin
      LReturnType := ReturnTypes[i];
      Break;
    end;
  end;

  if LReturnType = nil then
  begin
    i := Length(ReturnTypes);
    SetLength(ReturnTypes, i + 1);
    New(LReturnType);
    LReturnType^ := AReturnType;
    ReturnTypes[i] := LReturnType;
  end;

  SkipShortString(p);
  WriteProcessMemory(GetCurrentProcess, p, @LReturnType, SizeOf(Pointer), LByteCount);
  TRttiIntfMethod(AMethod).FReturnType := LReturnType^;
end;

constructor ReturnTypePatchAttribute.Create(ATypeInfo: PTypeInfo);
begin
  FReturnType := ATypeInfo;
end;

initialization

finalization
  Finalize;

end.