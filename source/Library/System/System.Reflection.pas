(*
  Copyright (c) 2011, Stefan Glienke
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

unit System.Reflection;

interface

uses
  Rtti,
  TypInfo;

type
  TRttiTypeHelper = class helper for TRttiType
  private
    function ExtractGenericArguments: string;
  public
    function GetGenericArguments: TArray<TRttiType>;
    function GetGenericTypeDefinition: string;
    function IsCovariantTo(OtherType: PTypeInfo): Boolean;
    function IsGenericTypeOf(const BaseTypeName: string): Boolean;
    function MakeGenericType(TypeArguments: array of PTypeInfo): TRttiType;
  end;

function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;

implementation

uses
  Classes,
  StrUtils,
  SysUtils,
  Types;

function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  LType := LContext.GetType(ThisClass);
  Result := LType.IsCovariantTo(OtherClass.ClassInfo);
end;

function MergeStrings(Values: TStringDynArray; const Delimiter: string): string;
var
  i: Integer;
begin
  for i := Low(Values) to High(Values) do
  begin
    if i = 0 then
    begin
      Result := Values[i];
    end
    else
    begin
      Result := Result + Delimiter + Values[i];
    end;
  end;
end;

{$IFDEF VER210}
function SplitString(const S: string; const Delimiter: Char): TStringDynArray;
var
  list: TStrings;
  i: Integer;
begin
  list := TStringList.Create();
  try
    list.StrictDelimiter := True;
    list.Delimiter := Delimiter;
    list.DelimitedText := s;
    SetLength(Result, list.Count);
    for i := Low(Result) to High(Result) do
    begin
      Result[i] := list[i];
    end;
  finally
    list.Free();
  end;
end;
{$ENDIF}

{ TRttiTypeHelper }

function TRttiTypeHelper.ExtractGenericArguments: string;
var
  i: Integer;
begin
  i := Pos('<', Name);
  if i > 0 then
  begin
    Result := Copy(Name, Succ(i), Length(Name) - Succ(i));
  end
  else
  begin
    Result := ''
  end;
end;

function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  i: Integer;
  args: TStringDynArray;
  ctx: TRttiContext;
begin
  args := SplitString(ExtractGenericArguments, ',');
  SetLength(Result, Length(args));
  for i := 0 to Pred(Length(args)) do
  begin
    Result[i] := ctx.FindType(args[i]);
  end;
end;

function TRttiTypeHelper.GetGenericTypeDefinition: string;
var
  i: Integer;
  args: TStringDynArray;
begin
  args := SplitString(ExtractGenericArguments, ',');
  for i := Low(args) to High(args) do
  begin
    // naive implementation - but will work in most cases
    if (i = 0) and (Length(args) = 1) then
    begin
      args[i] := 'T';
    end
    else
    begin
      args[i] := 'T' + IntToStr(Succ(i));
    end;
  end;
  if IsPublicType then
  begin
    Result := Copy(QualifiedName, 1, Pos('<', QualifiedName)) + MergeStrings(args, ',') + '>';
  end
  else
  begin
    Result := Copy(Name, 1, Pos('<', Name)) + MergeStrings(args, ',') + '>';
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherType: PTypeInfo): Boolean;
var
  ctx: TRttiContext;
  t: TRttiType;
  args, otherArgs: TArray<TRttiType>;
  i: Integer;
begin
  Result := True;
  t := ctx.GetType(OtherType);
  if Assigned(t) and (SameText(GetGenericTypeDefinition, t.GetGenericTypeDefinition)) then
  begin
    args := GetGenericArguments;
    otherArgs := t.GetGenericArguments;
    for i := Low(args) to High(args) do
    begin
      // only check for class types
      if not args[i].IsInstance or not otherArgs[i].IsInstance
        or not args[i].AsInstance.MetaclassType.InheritsFrom(
        otherArgs[i].AsInstance.MetaclassType) then
      begin
        Result := False;
        Break;
      end;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function TRttiTypeHelper.IsGenericTypeOf(const BaseTypeName: string): Boolean;
begin
  Result := (Copy(Name, 1, Succ(Length(BaseTypeName))) = (BaseTypeName + '<'))
    and (Copy(Name, Length(Name), 1) = '>');
end;

function TRttiTypeHelper.MakeGenericType(TypeArguments: array of PTypeInfo): TRttiType;
var
  i: Integer;
  args: TStringDynArray;
  ctx: TRttiContext;
  s: string;
begin
  if IsPublicType then
  begin
    args := SplitString(ExtractGenericArguments, ',');
    for i := Low(args) to High(args) do
    begin
      args[i] := ctx.GetType(TypeArguments[i]).QualifiedName;
    end;
    s := Copy(QualifiedName, 1, Pos('<', QualifiedName)) + MergeStrings(args, ',') + '>';
    Result := ctx.FindType(s);
  end
  else
  begin
    Result := nil;
  end;
end;

end.
