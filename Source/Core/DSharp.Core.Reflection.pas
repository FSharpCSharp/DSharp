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

unit DSharp.Core.Reflection;

interface

uses
  Rtti,
  TypInfo;

type
  TObjectHelper = class helper for TObject
  public
    function GetFields: TArray<TRttiField>;
    function GetField(const AName: string): TRttiField;
    function GetMethods: TArray<TRttiMethod>;
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;
    function GetMethod(const AName: string): TRttiMethod; overload;
    function GetProperties: TArray<TRttiProperty>;
    function GetProperty(const AName: string): TRttiProperty;
    function GetType: TRttiType;

    function HasField(const AName: string): Boolean;
    function HasMethod(const AName: string): Boolean;
    function HasProperty(const AName: string): Boolean;

    function TryGetField(const AName: string; out AField: TRttiField): Boolean;
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;
    function TryGetType(out AType: TRttiType): Boolean;
  end;

  TRttiFieldHelper = class helper for TRttiField
  public
    function TryGetValue(Instance: Pointer; out Value: TValue): Boolean;
  end;

{$IF COMPILERVERSION > 22}
  TRttiInvokableTypeHelper = class helper for TRttiInvokableType
  private
    function GetParameterCount: Integer;
  public
    property ParameterCount: Integer read GetParameterCount;
  end;
{$IFEND}

  TRttiMethodHelper = class helper for TRttiMethod
  private
    function GetParameterCount: Integer;
  public
    property ParameterCount: Integer read GetParameterCount;
  end;

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetAttributeOfType<T: TCustomAttribute>: T;
    function GetAttributesOfType<T: TCustomAttribute>: TArray<T>;

    function HasAttributeOfType<T: TCustomAttribute>: Boolean;

    function TryGetAttributeOfType<T: TCustomAttribute>(out AAttribute: T): Boolean;
  end;

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function TryGetValue(Instance: Pointer; out Value: TValue): Boolean;
  end;

  TRttiTypeHelper = class helper for TRttiType
  private
    function ExtractGenericArguments: string;
    function GetAsInterface: TRttiInterfaceType;
    function GetIsInterface: Boolean;
    function GetMethodCount: Integer;
    function InheritsFrom(OtherType: PTypeInfo): Boolean;
  public
    function GetAttributesOfType<T: TCustomAttribute>: TArray<T>;
    function GetGenericArguments: TArray<TRttiType>;
    function GetGenericTypeDefinition(const AIncludeUnitName: Boolean = True): string;
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;

    function IsCovariantTo(OtherClass: TClass): Boolean; overload;
    function IsCovariantTo(OtherType: PTypeInfo): Boolean; overload;
    function IsGenericTypeDefinition: Boolean;
    function IsGenericTypeOf(const BaseTypeName: string): Boolean;
    function MakeGenericType(TypeArguments: array of PTypeInfo): TRttiType;

    function TryGetField(const AName: string; out AField: TRttiField): Boolean;
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;

    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property IsInterface: Boolean read GetIsInterface;
    property MethodCount: Integer read GetMethodCount;
  end;

  TValueHelper = record helper for TValue
  private
    class function FromFloat(ATypeInfo: PTypeInfo; AValue: Extended): TValue; static;
  public
    function GetType: TRttiType;

    function IsFloat: Boolean;
    function IsNumeric: Boolean;
    function IsString: Boolean;

    // conversion for almost all standard types
    function TryConvert(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;

    function AsByte: Byte;
    function AsCardinal: Cardinal;
    function AsDate: TDate;
    function AsDateTime: TDateTime;
    function AsShortInt: ShortInt;
    function AsSmallInt: SmallInt;
    function AsTime: TTime;
    function AsUInt64: UInt64;
    function AsWord: Word;

    class function ToString(const Values: TArray<TValue>): string; overload; static;
    class function Equals(const Left, Right: TArray<TValue>): Boolean; static;

    class function FromBoolean(const Value: Boolean): TValue; static;
    class function FromString(const Value: string): TValue; static;

    function IsBoolean: Boolean;
    function IsByte: Boolean;
    function IsCardinal: Boolean;
    function IsDate: Boolean;
    function IsDateTime: Boolean;
    function IsInteger: Boolean;
    function IsInt64: Boolean;
    function IsShortInt: Boolean;
    function IsSmallInt: Boolean;
    function IsTime: Boolean;
    function IsUInt64: Boolean;
    function IsWord: Boolean;
  end;

function FindType(const AName: string; out AType: TRttiType): Boolean; overload;
function GetRttiType(AClass: TClass): TRttiType; overload;
function GetRttiType(ATypeInfo: PTypeInfo): TRttiType; overload;
function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
function TryGetRttiType(AClass: TClass; out AType: TRttiType): Boolean; overload;
function TryGetRttiType(ATypeInfo: PTypeInfo; out AType: TRttiType): Boolean; overload;

implementation

uses
  Classes,
  StrUtils,
  SysUtils,
  Types;

var
  Context: TRttiContext;

function FindType(const AName: string; out AType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  for LType in Context.GetTypes do
  begin
    if LType.Name = AName then
    begin
      AType := LType;
      Break;
    end;
  end;
  Result := Assigned(AType);
end;

function GetRttiType(AClass: TClass): TRttiType;
begin
  Result := Context.GetType(AClass);
end;

function GetRttiType(ATypeInfo: PTypeInfo): TRttiType;
begin
  Result := Context.GetType(ATypeInfo);
end;

function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisClass);
  Result := LType.IsCovariantTo(OtherClass.ClassInfo);
end;

function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisType);
  Result := LType.IsCovariantTo(OtherType);
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

function TryGetRttiType(AClass: TClass; out AType: TRttiType): Boolean; overload;
begin
  AType := Context.GetType(AClass);
  Result := Assigned(AType);
end;

function TryGetRttiType(ATypeInfo: PTypeInfo; out AType: TRttiType): Boolean; overload;
begin
  AType := Context.GetType(ATypeInfo);
  Result := Assigned(AType);
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

{ TObjectHelper }

function TObjectHelper.GetField(const AName: string): TRttiField;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetField(AName);
end;

function TObjectHelper.GetFields: TArray<TRttiField>;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetFields();
end;

function TObjectHelper.GetMethod(const AName: string): TRttiMethod;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetMethod(AName);
end;

function TObjectHelper.GetMethod(ACodeAddress: Pointer): TRttiMethod;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetMethod(ACodeAddress);
end;

function TObjectHelper.GetMethods: TArray<TRttiMethod>;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetMethods();
end;

function TObjectHelper.GetProperties: TArray<TRttiProperty>;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetProperties();
end;

function TObjectHelper.GetProperty(const AName: string): TRttiProperty;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetProperty(AName);
end;

function TObjectHelper.GetType: TRttiType;
begin
  TryGetType(Result);
end;

function TObjectHelper.HasField(const AName: string): Boolean;
begin
  Result := GetField(AName) <> nil;
end;

function TObjectHelper.HasMethod(const AName: string): Boolean;
begin
  Result := GetMethod(AName) <> nil;
end;

function TObjectHelper.HasProperty(const AName: string): Boolean;
begin
  Result := GetProperty(AName) <> nil;
end;

function TObjectHelper.TryGetField(const AName: string;
  out AField: TRttiField): Boolean;
begin
  AField := GetField(AName);
  Result := Assigned(AField);
end;

function TObjectHelper.TryGetMethod(ACodeAddress: Pointer;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(ACodeAddress);
  Result := Assigned(AMethod);
end;

function TObjectHelper.TryGetMethod(const AName: string;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(AName);
  Result := Assigned(AMethod);
end;

function TObjectHelper.TryGetProperty(const AName: string;
  out AProperty: TRttiProperty): Boolean;
begin
  AProperty := GetProperty(AName);
  Result := Assigned(AProperty);
end;

function TObjectHelper.TryGetType(out AType: TRttiType): Boolean;
begin
  Result := False;
  if Assigned(Self) then
  begin
    AType := Context.GetType(ClassInfo);
    Result := Assigned(AType);
  end;
end;

{ TRttiFieldHelper }

function TRttiFieldHelper.TryGetValue(Instance: Pointer;
  out Value: TValue): Boolean;
begin
  try
    Value := GetValue(Instance);
    Result := True;
  except
    Value := TValue.Empty;
    Result := False;
  end;
end;

{ TRttiInvokableTypeHelper }

{$IF COMPILERVERSION > 22}
function TRttiInvokableTypeHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters());
end;
{$IFEND}

{ TRttiMethodHelper }

function TRttiMethodHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters);
end;

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttributeOfType<T>: T;
var
  LAttribute: TCustomAttribute;
begin
  Result := Default(T);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      Result := T(LAttribute);
      Break;
    end;
  end;
end;

function TRttiObjectHelper.GetAttributesOfType<T>: TArray<T>;
var
  LAttribute: TCustomAttribute;
begin
  SetLength(Result, 0);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := T(LAttribute);
    end;
  end;
end;

function TRttiObjectHelper.HasAttributeOfType<T>: Boolean;
begin
  Result := GetAttributeOfType<T> <> nil;
end;

function TRttiObjectHelper.TryGetAttributeOfType<T>(out AAttribute: T): Boolean;
begin
  AAttribute := GetAttributeOfType<T>;
  Result := Assigned(AAttribute);
end;

{ TRttiPropertyHelper }

function TRttiPropertyHelper.TryGetValue(Instance: Pointer;
  out Value: TValue): Boolean;
begin
  try
    if IsReadable then
    begin
      Value := GetValue(Instance);
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  except
    Value := TValue.Empty;
    Result := False;
  end;
end;

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

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetAttributesOfType<T>: TArray<T>;
var
  LAttribute: TCustomAttribute;
  LAttributes: TArray<T>;
  i: Integer;
begin
  SetLength(Result, 0);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := T(LAttribute);
    end;
  end;

  if Assigned(BaseType) then
  begin
    for LAttribute in BaseType.GetAttributesOfType<T> do
    begin
      if LAttribute.InheritsFrom(T) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := T(LAttribute);
      end;
    end;
  end;
end;

function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  i: Integer;
  args: TStringDynArray;
begin
  args := SplitString(ExtractGenericArguments, ',');
  SetLength(Result, Length(args));
  for i := 0 to Pred(Length(args)) do
  begin
    Result[i] := Context.FindType(args[i]);
  end;
end;

function TRttiTypeHelper.GetGenericTypeDefinition(
  const AIncludeUnitName: Boolean = True): string;
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
  if IsPublicType and AIncludeUnitName then
  begin
    Result := Copy(QualifiedName, 1, Pos('<', QualifiedName)) + MergeStrings(args, ',') + '>';
  end
  else
  begin
    Result := Copy(Name, 1, Pos('<', Name)) + MergeStrings(args, ',') + '>';
  end;
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

function TRttiTypeHelper.GetMethod(ACodeAddress: Pointer): TRttiMethod;
var
  LMethod: TRttiMethod;
begin
  Result := nil;
  for LMethod in GetMethods() do
  begin
    if LMethod.CodeAddress = ACodeAddress then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

function TRttiTypeHelper.GetMethodCount: Integer;
begin
  Result := Length(GetMethods);
end;

function TRttiTypeHelper.InheritsFrom(OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  Result := Handle = OtherType;

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := LType.Handle = OtherType;
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherType: PTypeInfo): Boolean;
var
  t: TRttiType;
  args, otherArgs: TArray<TRttiType>;
  i: Integer;
begin
  Result := False;
  t := Context.GetType(OtherType);
  if Assigned(t) and IsGenericTypeDefinition then
  begin
    if SameText(GetGenericTypeDefinition, t.GetGenericTypeDefinition)
      or SameText(GetGenericTypeDefinition(False), t.GetGenericTypeDefinition(False)) then
    begin
      Result := True;
      args := GetGenericArguments;
      otherArgs := t.GetGenericArguments;
      for i := Low(args) to High(args) do
      begin
        if args[i].IsInterface and args[i].IsInterface
          and args[i].InheritsFrom(otherArgs[i].Handle) then
        begin
          Continue;
        end;

        if args[i].IsInstance and otherArgs[i].IsInstance
          and args[i].InheritsFrom(otherArgs[i].Handle) then
        begin
          Continue;
        end;

        Result := False;
        Break;
      end;
    end
    else
    begin
      if Assigned(BaseType) then
      begin
        Result := BaseType.IsCovariantTo(OtherType);
      end;
    end;
  end
  else
  begin
    Result := InheritsFrom(OtherType);
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherClass: TClass): Boolean;
begin
  Result := Assigned(OtherClass) and IsCovariantTo(OtherClass.ClassInfo);
end;

function TRttiTypeHelper.IsGenericTypeDefinition: Boolean;
begin
  Result := Length(GetGenericArguments) > 0;
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
  s: string;
begin
  if IsPublicType then
  begin
    args := SplitString(ExtractGenericArguments, ',');
    for i := Low(args) to High(args) do
    begin
      args[i] := Context.GetType(TypeArguments[i]).QualifiedName;
    end;
    s := Copy(QualifiedName, 1, Pos('<', QualifiedName)) + MergeStrings(args, ',') + '>';
    Result := Context.FindType(s);
  end
  else
  begin
    Result := nil;
  end;
end;

function TRttiTypeHelper.TryGetField(const AName: string;
  out AField: TRttiField): Boolean;
begin
  AField := GetField(AName);
  Result := Assigned(AField);
end;

function TRttiTypeHelper.TryGetMethod(ACodeAddress: Pointer;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(ACodeAddress);
  Result := Assigned(AMethod);
end;

function TRttiTypeHelper.TryGetMethod(const AName: string;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(AName);
  Result := Assigned(AMethod);
end;

function TRttiTypeHelper.TryGetProperty(const AName: string;
  out AProperty: TRttiProperty): Boolean;
begin
  AProperty := GetProperty(AName);
  Result := Assigned(AProperty);
end;

{ TValueHelper }

function TValueHelper.AsByte: Byte;
begin
  Result := AsType<Byte>;
end;

function TValueHelper.AsCardinal: Cardinal;
begin
  Result := AsType<Cardinal>;
end;

function TValueHelper.AsDate: TDate;
begin
  Result := AsType<TDate>;
end;

function TValueHelper.AsDateTime: TDateTime;
begin
  Result := AsType<TDateTime>;
end;

function TValueHelper.AsShortInt: ShortInt;
begin
  Result := AsType<ShortInt>;
end;

function TValueHelper.AsSmallInt: SmallInt;
begin
  Result := AsType<SmallInt>;
end;

function TValueHelper.AsTime: TTime;
begin
  Result := AsType<TTime>;
end;

function TValueHelper.AsUInt64: UInt64;
begin
  Result := AsType<UInt64>;
end;

function TValueHelper.AsWord: Word;
begin
  Result := AsType<Word>;
end;

class function TValueHelper.Equals(const Left, Right: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  Result := Length(Left) = Length(Left);
  if Result then
  begin
    for i := Low(Left) to High(Left) do
    begin
      if (Left[i].TypeInfo <> Right[i].TypeInfo)
        or (Left[i].ToString <> Right[i].ToString) then
      begin
        Result := False;
        Break;
      end;
    end
  end;
end;

class function TValueHelper.FromBoolean(const Value: Boolean): TValue;
begin
  Result := TValue.From<Boolean>(Value);
end;

class function TValueHelper.FromFloat(ATypeInfo: PTypeInfo;
  AValue: Extended): TValue;
begin
  case GetTypeData(ATypeInfo).FloatType of
    ftSingle: Result := TValue.From<Single>(AValue);
    ftDouble: Result := TValue.From<Double>(AValue);
    ftExtended: Result := TValue.From<Extended>(AValue);
    ftComp: Result := TValue.From<Comp>(AValue);
    ftCurr: Result := TValue.From<Currency>(AValue);
  end;
end;

class function TValueHelper.FromString(const Value: string): TValue;
begin
  Result := TValue.From<string>(Value);
end;

function TValueHelper.GetType: TRttiType;
begin
  Result := Context.GetType(TypeInfo);
end;

function TValueHelper.IsBoolean: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Boolean);
end;

function TValueHelper.IsByte: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Byte);
end;

function TValueHelper.IsCardinal: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Cardinal);
end;

function TValueHelper.IsDate: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDate);
end;

function TValueHelper.IsDateTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDateTime);
end;

function TValueHelper.IsFloat: Boolean;
begin
  Result := Kind = tkFloat;
end;

function TValueHelper.IsInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Int64);
end;

function TValueHelper.IsInteger: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Integer);
end;

function TValueHelper.IsNumeric: Boolean;
begin
  Result := Kind in [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
end;

function TValueHelper.IsShortInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(ShortInt);
end;

function TValueHelper.IsSmallInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(SmallInt);
end;

function TValueHelper.IsString: Boolean;
begin
  Result := Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString];
end;

function TValueHelper.IsTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TTime);
end;

function TValueHelper.IsUInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(UInt64);
end;

function TValueHelper.IsWord: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Word);
end;

class function TValueHelper.ToString(const Values: TArray<TValue>): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Values) to High(Values) do
  begin
    if i > Low(Values) then
    begin
      Result := Result + ', ';
    end;
    Result := Result + Values[i].ToString;
  end;
end;

function TValueHelper.TryConvert(ATypeInfo: PTypeInfo;
  out AResult: TValue): Boolean;
begin
  Result := False;
  case Kind of
    tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
    begin
      case ATypeInfo.Kind of
        tkInteger, tkEnumeration, tkChar, tkInt64:
        begin
          AResult := TValue.FromOrdinal(ATypeInfo, AsOrdinal);
          Result := True;
        end;
        tkFloat:
        begin
          AResult := TValue.FromFloat(ATypeInfo, AsOrdinal);
          Result := True;
        end;
        tkUString:
        begin
          if IsBoolean then
          begin
            AResult := TValue.FromString(BoolToStr(AsBoolean, True));
            Result := True;
          end
          else
          begin
            if IsInt64 then
            begin
              AResult := TValue.FromString(IntToStr(AsInt64));
              Result := True;
            end else
            if IsInteger then
            begin
              AResult := TValue.FromString(IntToStr(AsInteger));
              Result := True;
            end else
            if IsSmallInt then
            begin
              AResult := TValue.FromString(IntToStr(AsSmallInt));
              Result := True;
            end else
            if IsShortInt then
            begin
              AResult := TValue.FromString(IntToStr(AsShortInt));
              Result := True;
            end else
            if IsUInt64 then
            begin
              AResult := TValue.FromString(UIntToStr(AsUInt64));
              Result := True;
            end else
            if IsCardinal then
            begin
              AResult := TValue.FromString(UIntToStr(AsCardinal));
              Result := True;
            end else
            if IsWord then
            begin
              AResult := TValue.FromString(UIntToStr(AsWord));
              Result := True;
            end else
            if IsByte then
            begin
              AResult := TValue.FromString(UIntToStr(AsByte));
              Result := True;
            end;
          end;
        end;
      end;
    end;
    tkFloat:
    begin
      case ATypeInfo.Kind of
        tkInteger, tkInt64:
        begin
          Result := Frac(AsExtended) = 0;
          if Result then
          begin
            AResult := TValue.FromOrdinal(ATypeInfo, Trunc(AsExtended));
          end;
        end;
        tkUString:
        begin
          if IsDate then
          begin
            AResult := TValue.FromString(DateToStr(AsDate));
            Result := True;
          end
          else
          if IsDateTime then
          begin
            AResult := TValue.FromString(DateTimeToStr(AsDateTime));
            Result := True;
          end
          else
          if IsTime then
          begin
            AResult := TValue.FromString(TimeToStr(AsTime));
            Result := True;
          end
          else
          begin
            AResult := TValue.FromString(FloatToStr(AsExtended));
            Result := True;
          end;
        end;
      end;
    end;
    tkUString:
    begin
      case ATypeInfo.Kind of
        tkInteger, tkEnumeration, tkChar, tkInt64:
        begin
          if ATypeInfo = System.TypeInfo(Boolean) then
          begin
            AResult := TValue.FromBoolean(StrToBoolDef(AsString, False));
            Result := True;
          end
          else
          begin
            AResult := TValue.FromOrdinal(ATypeInfo, StrToIntDef(AsString, 0));
            Result := True;
          end;
        end;
        tkWChar:
        begin
          Result := Length(AsString) = 1;
          if Result then
          begin
            AResult := TValue.From<Char>(AsString[1]);
          end;
        end;
        tkFloat:
        begin
          if ATypeInfo = System.TypeInfo(TDate) then
          begin
            AResult := TValue.From<TDate>(StrToDateDef(AsString, 0));
            Result := True;
          end
          else
          if ATypeInfo = System.TypeInfo(TDateTime) then
          begin
            AResult := TValue.From<TDateTime>(StrToDateTimeDef(AsString, 0));
            Result := True;
          end
          else
          if ATypeInfo = System.TypeInfo(TTime) then
          begin
            AResult := TValue.From<TTime>(StrToTimeDef(AsString, 0));
            Result := True;
          end
          else
          begin
            AResult := TValue.FromFloat(ATypeInfo, StrToFloatDef(AsString, 0));
            Result := True;
          end;
        end;
      end;
    end;
    tkClass:
    begin
      case ATypeInfo.Kind of
        tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
        begin
          if ATypeInfo = System.TypeInfo(Boolean) then
          begin
            AResult := TValue.FromBoolean(AsObject <> nil);
            Result := True;
          end
          else
          begin
            AResult := TValue.FromOrdinal(ATypeInfo, Int64(AsObject));
            Result := True;
          end;
        end;
      end;
    end;
    tkRecord:
    begin
      case ATypeInfo.Kind of
        tkMethod:
        begin
          if TypeInfo = System.TypeInfo(System.TMethod) then
          begin
            TValue.Make(GetReferenceToRawData, ATypeInfo, AResult);
            Result := True;
          end;
        end;
      end;
    end;
{$IFDEF VER210}
    // workaround for bug in RTTI.pas (fixed in XE)
    tkUnknown:
    begin
      case ATypeInfo.Kind of
        tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
        begin
          AResult := TValue.FromOrdinal(ATypeInfo, 0);
          Result := True;
        end;
        tkFloat:
        begin
          AResult := TValue.From<Extended>(0);
          Result := True;
        end;
        tkUString:
        begin
          AResult := TValue.FromString('');
          Result := True;
        end;
      end;
    end;
{$ENDIF}
  end;
  if not Result then
  begin
    Result := TryCast(ATypeInfo, AResult);
  end;
end;

end.
