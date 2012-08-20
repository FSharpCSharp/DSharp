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

unit DSharp.Core.Reflection;

interface

uses
  Rtti,
  Types,
  TypInfo;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.TObject">TObject</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TObjectHelper = class helper for TObject
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns a list of all fields of the object.
    ///	</summary>
    {$ENDREGION}
    function GetFields: TArray<TRttiField>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the field with the given name; <b>nil</b> if nothing is found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    {$ENDREGION}
    function GetField(const AName: string): TRttiField;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns a list of all methods of the object.
    ///	</summary>
    {$ENDREGION}
    function GetMethods: TArray<TRttiMethod>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method at the given code address; <b>nil</b> if nothing
    ///	  is found.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find.
    ///	</param>
    {$ENDREGION}
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method with the given name; <b>nil</b> if nothing is
    ///	  found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    {$ENDREGION}
    function GetMethod(const AName: string): TRttiMethod; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns a list of all properties of the object.
    ///	</summary>
    {$ENDREGION}
    function GetProperties: TArray<TRttiProperty>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the property with the given name; <b>nil</b> if nothing is
    ///	  found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    {$ENDREGION}
    function GetProperty(const AName: string): TRttiProperty;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the type of the object; nil if nothing is found.
    ///	</summary>
    {$ENDREGION}
    function GetType: TRttiType;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns if the object contains a field with the given name.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    {$ENDREGION}
    function HasField(const AName: string): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns if the object contains a method with the given name.
    ///	</summary>
    {$ENDREGION}
    function HasMethod(const AName: string): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns if the object contains a property with the given name.
    ///	</summary>
    {$ENDREGION}
    function HasProperty(const AName: string): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    ///	<param name="AField">
    ///	  Field that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetField(const AName: string; out AField: TRttiField): Boolean;

    {$REGION 'Documentation'}
    ///	<param name="AName">
    ///	  Name of the�member to find
    ///	</param>
    ///	<param name="AMember">
    ///	  Member that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMember(const AName: string; out AMember: TRttiMember): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the�property with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    ///	<param name="AProperty">
    ///	  Property that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the type of the object and returns if this was successful.
    ///	</summary>
    ///	<param name="AType">
    ///	  Type of the object when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetType(out AType: TRttiType): Boolean;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends�<see cref="Rtti.TRttiField">TRttiField</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiFieldHelper = class helper for TRttiField
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the�value of the field and returns if this was successful.
    ///	</summary>
    ///	<param name="Instance">
    ///	  Pointer to the instance of the field
    ///	</param>
    ///	<param name="Value">
    ///	  Value of the field when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetValue(Instance: Pointer; out Value: TValue): Boolean;
  end;

{$IF CompilerVersion < 23}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends�<see cref="Rtti.TRttiInstanceTypeHelper">TRttiInstanceTypeHelper</see>
  ///	  for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiInstanceTypeHelper = class helper for TRttiInstanceType
  public
    function GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
    function GetImplementedInterfaces: TArray<TRttiInterfaceType>;
  end;
{$IFEND}

{$IF CompilerVersion > 22}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends�<see cref="Rtti.TRttiInvokableType">TRttiInvokableType</see>
  ///	  for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiInvokableTypeHelper = class helper for TRttiInvokableType
  private
    function GetParameterCount: Integer;
  public
    property ParameterCount: Integer read GetParameterCount;
  end;
{$IFEND}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiMember">TRttiMember</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsReadable: Boolean;
    function GetIsWritable: Boolean;
    function GetRttiType: TRttiType;
  public
    property IsReadable: Boolean read GetIsReadable;
    property IsWritable: Boolean read GetIsWritable;
    property RttiType: TRttiType read GetRttiType;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiMethod">TRttiMethod</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiMethodHelper = class helper for TRttiMethod
  private
    function GetParameterCount: Integer;
  public
    function Format(const Args: array of TValue; SkipSelf: Boolean = True): string;
    property ParameterCount: Integer read GetParameterCount;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiObject">TRttiObject</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetAttributeOfType<T: TCustomAttribute>: T;
    function GetAttributesOfType<T: TCustomAttribute>: TArray<T>;

    function HasAttributeOfType<T: TCustomAttribute>: Boolean;

    function TryGetAttributeOfType<T: TCustomAttribute>(out AAttribute: T): Boolean;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiParameter">TRttiParameter</see> for easier
  ///	  RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiParameterHelper = class helper for TRttiParameter
  public
    class function Equals(const Left, Right: TArray<TRttiParameter>): Boolean; //overload;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiProperty">TRttiProperty</see> for easier
  ///	  RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiPropertyHelper = class helper for TRttiProperty
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the value of the�property and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="Instance">
    ///	  Pointer to the instance of the field
    ///	</param>
    ///	<param name="Value">
    ///	  Value of the field when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetValue(Instance: Pointer; out Value: TValue): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Sets�the value of the property and returns if this was successful.
    ///	</summary>
    ///	<param name="Instance">
    ///	  Pointer to the instance of the field
    ///	</param>
    ///	<param name="Value">
    ///	  Value the field should be set to
    ///	</param>
    {$ENDREGION}
    function TrySetValue(Instance: Pointer; Value: TValue): Boolean;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiType">TRttiType</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
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

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method at the given code address; <b>nil</b> if nothing
    ///	  is found.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    {$ENDREGION}
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;

    function GetStandardConstructor: TRttiMethod;

    function IsCovariantTo(OtherClass: TClass): Boolean; overload;
    function IsCovariantTo(OtherType: PTypeInfo): Boolean; overload;
    function IsGenericTypeDefinition: Boolean;
    function IsGenericTypeOf(const BaseTypeName: string): Boolean;
    function IsInheritedFrom(OtherType: TRttiType): Boolean; overload;
    function IsInheritedFrom(const OtherTypeName: string): Boolean; overload;
    function MakeGenericType(TypeArguments: array of PTypeInfo): TRttiType;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    ///	<param name="AField">
    ///	  Field that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetField(const AName: string; out AField: TRttiField): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the property with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    ///	<param name="AProperty">
    ///	  Property that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;

    function TryGetStandardConstructor(out AMethod: TRttiMethod): Boolean;

    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property IsInterface: Boolean read GetIsInterface;
    property MethodCount: Integer read GetMethodCount;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TValue">TValue</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TValueHelper = record helper for TValue
  private
    class function FromFloat(ATypeInfo: PTypeInfo; AValue: Extended): TValue; static;
  public
    function GetType: TRttiType;

    function IsFloat: Boolean;
    function IsNumeric: Boolean;
    function IsPointer: Boolean;
    function IsString: Boolean;

    function IsInstance: Boolean;
    function IsInterface: Boolean;

    // conversion for almost all standard types
    function TryConvert(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;

    function AsByte: Byte;
    function AsCardinal: Cardinal;
    function AsCurrency: Currency;
    function AsDate: TDate;
    function AsDateTime: TDateTime;
    function AsDouble: Double;
    function AsFloat: Extended;
    function AsPointer: Pointer;
    function AsShortInt: ShortInt;
    function AsSingle: Single;
    function AsSmallInt: SmallInt;
    function AsTime: TTime;
    function AsUInt64: UInt64;
    function AsWord: Word;

    function ToObject: TObject;

    class function ToString(const Value: TValue): string; overload; static;
    class function ToString(const Values: TArray<TValue>): string; overload; static;
    class function Equals(const Left, Right: TArray<TValue>): Boolean; overload; static;
    class function Equals<T>(const Left, Right: T): Boolean; overload; static;

    class function From(ABuffer: Pointer; ATypeInfo: PTypeInfo): TValue; overload; static;
    class function FromBoolean(const Value: Boolean): TValue; static;
    class function FromString(const Value: string): TValue; static;

    function IsBoolean: Boolean;
    function IsByte: Boolean;
    function IsCardinal: Boolean;
    function IsCurrency: Boolean;
    function IsDate: Boolean;
    function IsDateTime: Boolean;
    function IsDouble: Boolean;
    function IsInteger: Boolean;
    function IsInt64: Boolean;
    function IsShortInt: Boolean;
    function IsSingle: Boolean;
    function IsSmallInt: Boolean;
    function IsTime: Boolean;
    function IsUInt64: Boolean;
    function IsVariant: Boolean;
    function IsWord: Boolean;
  end;

  PObject = ^TObject;

function FindType(const AName: string; out AType: TRttiType): Boolean; overload;
function FindType(const AGuid: TGUID; out AType: TRttiType): Boolean; overload;

{$REGION 'Documentation'}
///	<summary>
///	  Returns the RTTI type of the given TClass.
///	</summary>
{$ENDREGION}
function GetRttiType(AClass: TClass): TRttiType; overload;

{$REGION 'Documentation'}
///	<summary>
///	  Returns the RTTI type of the given TypeInfo.
///	</summary>
{$ENDREGION}
function GetRttiType(ATypeInfo: PTypeInfo): TRttiType; overload;
function GetRttiTypes: TArray<TRttiType>;
function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
function TryGetRttiType(AClass: TClass; out AType: TRttiType): Boolean; overload;
function TryGetRttiType(ATypeInfo: PTypeInfo; out AType: TRttiType): Boolean; overload;

function CompareValue(const Left, Right: TValue): Integer;
function SameValue(const Left, Right: TValue): Boolean;

function StripUnitName(const s: string): string;

{$IFDEF VER210}
function SplitString(const S: string; const Delimiter: Char): TStringDynArray;
{$ENDIF}

function Supports(const Instance: TValue; const IID: TGUID; out Intf): Boolean; overload;

const
  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';

implementation

uses
  Classes,
  Generics.Collections,
  Generics.Defaults,
  Math,
  StrUtils,
  SysUtils;

type
  TArrayHelper = class
  public
    class function Concat<T>(const Arrays: array of TArray<T>): TArray<T>; static;
  end;

var
  Context: TRttiContext;
  Enumerations: TDictionary<PTypeInfo, TStrings>;

function FindType(const AName: string; out AType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  AType := Context.FindType(AName);
  if not Assigned(AType) then
  begin
    for LType in Context.GetTypes do
    begin
      if SameText(LType.Name, AName) then
      begin
        AType := LType;
        Break;
      end;
    end;
  end;
  Result := Assigned(AType);
end;

function FindType(const AGuid: TGUID; out AType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  AType := nil;
  for LType in Context.GetTypes do
  begin
    if (LType is TRttiInterfaceType)
      and IsEqualGUID(TRttiInterfaceType(LType).GUID, AGuid) then
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

function GetRttiTypes: TArray<TRttiType>;
begin
  Result := Context.GetTypes();
end;

function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisClass);
  Result := Assigned(LType) and LType.IsCovariantTo(OtherClass.ClassInfo);
end;

function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisType);
  Result := Assigned(LType) and LType.IsCovariantTo(OtherType);
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

function StripUnitName(const s: string): string;
begin
  Result := ReplaceText(s, 'System.', '');
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

function Supports(const Instance: TValue; const IID: TGUID; out Intf): Boolean; overload;
begin
  if Instance.Kind in [tkClass, tkInterface] then
  begin
    Result := Supports(Instance.ToObject, IID, Intf);
  end
  else
  begin
    Result := False;
  end;
end;

function CompareValue(const Left, Right: TValue): Integer;
begin
  if Left.IsOrdinal and Right.IsOrdinal then
  begin
    Result := Math.CompareValue(Left.AsOrdinal, Right.AsOrdinal);
  end else
  if Left.IsFloat and Right.IsFloat then
  begin
    Result := Math.CompareValue(Left.AsFloat, Right.AsFloat);
  end else
  if Left.IsString and Right.IsString then
  begin
    Result := SysUtils.CompareStr(Left.AsString, Right.AsString);
  end else
  begin
    Result := 0;
  end;
end;

function SameValue(const Left, Right: TValue): Boolean;
begin
  if Left.IsNumeric and Right.IsNumeric then
  begin
    if Left.IsOrdinal then
    begin
      if Right.IsOrdinal then
      begin
        Result := Left.AsOrdinal = Right.AsOrdinal;
      end else
      if Right.IsSingle then
      begin
        Result := Math.SameValue(Left.AsOrdinal, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := Math.SameValue(Left.AsOrdinal, Right.AsDouble);
      end
      else
      begin
        Result := Math.SameValue(Left.AsOrdinal, Right.AsExtended);
      end;
    end else
    if Left.IsSingle then
    begin
      if Right.IsOrdinal then
      begin
        Result := Math.SameValue(Left.AsSingle, Right.AsOrdinal);
      end else
      if Right.IsSingle then
      begin
        Result := Math.SameValue(Left.AsSingle, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := Math.SameValue(Left.AsSingle, Right.AsDouble);
      end
      else
      begin
        Result := Math.SameValue(Left.AsSingle, Right.AsExtended);
      end;
    end else
    if Left.IsDouble then
    begin
      if Right.IsOrdinal then
      begin
        Result := Math.SameValue(Left.AsDouble, Right.AsOrdinal);
      end else
      if Right.IsSingle then
      begin
        Result := Math.SameValue(Left.AsDouble, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := Math.SameValue(Left.AsDouble, Right.AsDouble);
      end
      else
      begin
        Result := Math.SameValue(Left.AsDouble, Right.AsExtended);
      end;
    end
    else
    begin
      if Right.IsOrdinal then
      begin
        Result := Math.SameValue(Left.AsExtended, Right.AsOrdinal);
      end else
      if Right.IsSingle then
      begin
        Result := Math.SameValue(Left.AsExtended, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := Math.SameValue(Left.AsExtended, Right.AsDouble);
      end
      else
      begin
        Result := Math.SameValue(Left.AsExtended, Right.AsExtended);
      end;
    end;
  end else
  if Left.IsString and Right.IsString then
  begin
    Result := Left.AsString = Right.AsString;
  end else
  if Left.IsClass and Right.IsClass then
  begin
    Result := Left.AsClass = Right.AsClass;
  end else
  if Left.IsObject and Right.IsObject then
  begin
    Result := Left.AsObject = Right.AsObject;
  end else
  if Left.IsPointer and Right.IsPointer then
  begin
    Result := Left.AsPointer = Right.AsPointer;
  end else
  if Left.IsVariant and Right.IsVariant then
  begin
    Result := Left.AsVariant = Right.AsVariant;
  end else
  if Left.TypeInfo = Right.TypeInfo then
  begin
    Result := Left.AsPointer = Right.AsPointer;
  end else
  begin
    Result := False;
  end;
end;

{ TArrayHelper }

class function TArrayHelper.Concat<T>(
  const Arrays: array of TArray<T>): TArray<T>;
var
  i, k, LIndex, LLength: Integer;
begin
  LLength := 0;
  for i := 0 to High(Arrays) do
    Inc(LLength, Length(Arrays[i]));
  SetLength(Result, LLength);
  LIndex := 0;
  for i := 0 to High(Arrays) do
  begin
    for k := 0 to High(Arrays[i]) do
    begin
      Result[LIndex] := Arrays[i][k];
      Inc(LIndex);
    end;
  end;
end;

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
  try
    Result := LType.GetMethod(AName);
  except
    Result := nil;
  end;
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

function TObjectHelper.TryGetMember(const AName: string;
  out AMember: TRttiMember): Boolean;
var
  LProperty: TRttiProperty;
  LField: TRttiField;
  LMethod: TRttiMethod;
begin
  if TryGetProperty(AName, LProperty) then
  begin
    Result := True;
    AMember := LProperty;
  end else
  if TryGetField(AName, LField) then
  begin
    Result := True;
    AMember := LField;
  end else
  if TryGetMethod(AName, LMethod) then
  begin
    Result := True;
    AMember := LMethod;
  end else
  begin
    Result := False;
    AMember := nil
  end;
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

{ TRttiInstanceTypeHelper }

{$IF CompilerVersion < 23}
function TRttiInstanceTypeHelper.GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
var
  LInterfaceTable: PInterfaceTable;
  p: PPointer;
  i: Integer;
  LTypeInfo: PTypeInfo;
begin
  LInterfaceTable := PPointer(PByte(MetaclassType) + vmtIntfTable)^;

  if Assigned(LInterfaceTable) then
  begin
    p := @LInterfaceTable.Entries[LInterfaceTable.EntryCount];
    SetLength(Result, LInterfaceTable.EntryCount);

    for i := 0 to LInterfaceTable.EntryCount - 1 do
    begin
      LTypeInfo := PPTypeInfo(p^)^;
      Result[i] := GetRttiType(LTypeInfo) as TRttiInterfaceType;
      Inc(p);
    end;
  end;
end;

function TRttiInstanceTypeHelper.GetImplementedInterfaces: TArray<TRttiInterfaceType>;
var
  LCount: Integer;
  LInterfaces: TArray<TArray<TRttiInterfaceType>>;
  LType: TRttiInstanceType;
begin
  LCount := 0;
  LType := Self;
  repeat
    Inc(LCount);
    LType := LType.BaseType.AsInstance;
  until not Assigned(LType);

  SetLength(LInterfaces, LCount);
  LCount := 0;
  LType := Self;
  repeat
    LInterfaces[LCount] := LType.GetDeclaredImplementedInterfaces;
    Inc(LCount);
    LType := LType.BaseType.AsInstance;
  until not Assigned(LType);

  Result := TArrayHelper.Concat<TRttiInterfaceType>(LInterfaces);
end;
{$IFEND}

{ TRttiInvokableTypeHelper }

{$IF CompilerVersion > 22}
function TRttiInvokableTypeHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters());
end;
{$IFEND}

{ TRttiMemberHelper }

function TRttiMemberHelper.GetIsReadable: Boolean;
begin
  Result := True;
  if Self is TRttiField then
  begin
    Result := True;
  end else
  if Self is TRttiProperty then
  begin
    Result := TRttiProperty(Self).IsReadable;
  end else
  if Self is TRttiMethod then
  begin
    Result := True;
  end;
end;

function TRttiMemberHelper.GetIsWritable: Boolean;
begin
  Result := False;
  if Self is TRttiField then
  begin
    Result := True;
  end else
  if Self is TRttiProperty then
  begin
    Result := TRttiProperty(Self).IsWritable;
  end;
end;

function TRttiMemberHelper.GetRttiType: TRttiType;
begin
  Result := nil;
  if Self is TRttiField then
  begin
    Result := TRttiField(Self).FieldType;
  end else
  if Self is TRttiProperty then
  begin
    Result := TRttiProperty(Self).PropertyType;
  end else
  if Self is TRttiMethod then
  begin
    Result := TRttiMethod(Self).ReturnType;
  end;
end;

{ TRttiMethodHelper }

function TRttiMethodHelper.Format(const Args: array of TValue;
  SkipSelf: Boolean): string;
begin
  Result := StripUnitName(Parent.Name) + '.' + Name + '(';
  if SkipSelf then
  begin
    if Length(Args) > 1 then
    begin
      Result := Result + TValue.ToString(@Args[1]);
    end;
  end
  else
  begin
    Result := Result + TValue.ToString(@Args[0])
  end;
  Result := Result + ')';
end;

function TRttiMethodHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters());
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

{ TRttiParameterHelper }

class function TRttiParameterHelper.Equals(const Left,
  Right: TArray<TRttiParameter>): Boolean;
var
  i: Integer;
begin
  Result := Length(Left) = Length(Left);
  if Result then
  begin
    for i := Low(Left) to High(Left) do
    begin
      if Left[i].ParamType <> Right[i].ParamType then
      begin
        Result := False;
        Break;
      end;
    end
  end;
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

function TRttiPropertyHelper.TrySetValue(Instance: Pointer;
  Value: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := Value.TryConvert(PropertyType.Handle, LValue);
  if Result then
  begin
    SetValue(Instance, LValue);
  end;
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.ExtractGenericArguments: string;
var
  i: Integer;
  s: string;
begin
  s := Name;
  i := Pos('<', s);
  if i > 0 then
  begin
    Result := Copy(s, Succ(i), Length(s) - Succ(i));
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
    FindType(args[i], Result[i]);
  end;
end;

function TRttiTypeHelper.GetGenericTypeDefinition(
  const AIncludeUnitName: Boolean = True): string;
var
  i: Integer;
  args: TStringDynArray;
  s: string;
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
    s := QualifiedName;
    Result := Copy(s, 1, Pos('<', s)) + MergeStrings(args, ',') + '>';
  end
  else
  begin
    s := Name;
    Result := Copy(s, 1, Pos('<', s)) + MergeStrings(args, ',') + '>';
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

function TRttiTypeHelper.GetStandardConstructor: TRttiMethod;
var
  LMethod: TRttiMethod;
begin
  Result := nil;
  for LMethod in GetMethods do
  begin
    if LMethod.IsConstructor and (LMethod.ParameterCount = 0) then
    begin
      Result := LMethod;
      Break;
    end;
  end;
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
  if not Result and Assigned(BaseType) then
  begin
    Result := BaseType.IsGenericTypeDefinition;
  end;
end;

function TRttiTypeHelper.IsGenericTypeOf(const BaseTypeName: string): Boolean;
var
  s: string;
begin
  s := Name;
  Result := (Copy(s, 1, Succ(Length(BaseTypeName))) = (BaseTypeName + '<'))
    and (Copy(s, Length(s), 1) = '>');
end;

function TRttiTypeHelper.IsInheritedFrom(const OtherTypeName: string): Boolean;
var
  LType: TRttiType;
begin
  Result := SameText(Name, OtherTypeName)
    or (IsPublicType and SameText(QualifiedName, OtherTypeName));

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := SameText(LType.Name, OtherTypeName)
        or (LType.IsPublicType and SameText(LType.QualifiedName, OtherTypeName));
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.IsInheritedFrom(OtherType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  Result := Self.Handle = OtherType.Handle;

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := LType.Handle = OtherType.Handle;
      LType := LType.BaseType;
    end;
  end;
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

function TRttiTypeHelper.TryGetStandardConstructor(
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetStandardConstructor();
  Result := Assigned(AMethod);
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

function TValueHelper.AsCurrency: Currency;
begin
  Result := AsType<Currency>;
end;

function TValueHelper.AsDate: TDate;
begin
  Result := AsType<TDate>;
end;

function TValueHelper.AsDateTime: TDateTime;
begin
  Result := AsType<TDateTime>;
end;

function TValueHelper.AsDouble: Double;
begin
  Result := AsType<Double>;
end;

function TValueHelper.AsFloat: Extended;
begin
  Result := AsType<Extended>;
end;

function TValueHelper.AsPointer: Pointer;
begin
  ExtractRawDataNoCopy(@Result);
end;

function TValueHelper.AsShortInt: ShortInt;
begin
  Result := AsType<ShortInt>;
end;

function TValueHelper.AsSingle: Single;
begin
  Result := AsType<Single>;
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
      if not SameValue(Left[i], Right[i]) then
      begin
        Result := False;
        Break;
      end;
    end
  end;
end;

class function TValueHelper.Equals<T>(const Left, Right: T): Boolean;
begin
  Result := TEqualityComparer<T>.Default.Equals(Left, Right);
end;

class function TValueHelper.From(ABuffer: Pointer;
  ATypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(ABuffer, ATypeInfo, Result);
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
{$IFNDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeUInt));
{$ENDIF}
end;

function TValueHelper.IsCurrency: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Currency);
end;

function TValueHelper.IsDate: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDate);
end;

function TValueHelper.IsDateTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDateTime);
end;

function TValueHelper.IsDouble: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Double);
end;

function TValueHelper.IsFloat: Boolean;
begin
  Result := Kind = tkFloat;
end;

function TValueHelper.IsInstance: Boolean;
begin
  Result := Kind in [tkClass, tkInterface];
end;

function TValueHelper.IsInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Int64);
{$IFDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsInteger: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Integer);
{$IFNDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsInterface: Boolean;
begin
  Result := Assigned(TypeInfo) and (TypeInfo.Kind = tkInterface);
end;

function TValueHelper.IsNumeric: Boolean;
begin
  Result := Kind in [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
end;

function TValueHelper.IsPointer: Boolean;
begin
  Result := Kind = tkPointer;
end;

function TValueHelper.IsShortInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(ShortInt);
end;

function TValueHelper.IsSingle: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Single);
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
{$IFDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsVariant: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Variant);
end;

function TValueHelper.IsWord: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Word);
end;

function TValueHelper.ToObject: TObject;
begin
  if IsInterface then
    Result := AsInterface as TObject
  else
    Result := AsObject;
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
    if Values[i].IsString then
    begin
      Result := Result + '''' + TValue.ToString(Values[i]) + '''';
    end
    else
    begin
      Result := Result + TValue.ToString(Values[i]);
    end;
  end;
end;

class function TValueHelper.ToString(const Value: TValue): string;
var
  LInterface: IInterface;
  LObject: TObject;
begin
  case Value.Kind of
    tkFloat:
    begin
      if Value.IsDate then
      begin
        Result := DateToStr(Value.AsDate);
      end else
      if Value.IsDateTime then
      begin
        Result := DateTimeToStr(Value.AsDateTime);
      end else
      if Value.IsTime then
      begin
        Result := TimeToStr(Value.AsTime);
      end else
      begin
        Result := Value.ToString;
      end;
    end;
    tkClass:
    begin
      LObject := Value.AsObject;
      Result := Format('%s($%x)', [StripUnitName(LObject.ClassName),
        NativeInt(LObject)]);
    end;
    tkInterface:
    begin
      LInterface := Value.AsInterface;
      LObject := LInterface as TObject;
      Result := Format('%s($%x) as %s', [StripUnitName(LObject.ClassName),
        NativeInt(LInterface), StripUnitName(string(Value.TypeInfo.Name))]);
    end
  else
    Result := Value.ToString;
  end;
end;

function TValueHelper.TryConvert(ATypeInfo: PTypeInfo;
  out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LInterface: IInterface;
  LStrings: TStrings;
  LTypeData: PTypeData;
  i: Integer;
begin
  Result := False;
  if Assigned(ATypeInfo) then
  begin
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
            end else
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
            end else
            begin
              AResult := TValue.FromString(ToString);
              Result := True;
            end;
          end;
          tkClass:
          begin
            LType := GetRttiType(ATypeInfo);
            if (Kind = tkEnumeration)
              and LType.AsInstance.MetaclassType.InheritsFrom(TStrings) then
            begin
              if not Enumerations.TryGetValue(TypeInfo, LStrings) then
              begin
                LStrings := TStringList.Create;
                with TRttiEnumerationType(GetType()) do
                begin
                  for i := MinValue to MaxValue do
                  begin
                    LStrings.Add(GetEnumName(Handle, i));
                  end;
                end;
                Enumerations.Add(TypeInfo, LStrings);
              end;
              AResult := TValue.From<TStrings>(LStrings);
              Result := True;
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
      tkSet:
      begin
        case ATypeInfo.Kind of
          tkClass:
          begin
            LType := GetRttiType(ATypeInfo);
            if LType.AsInstance.MetaclassType.InheritsFrom(TStrings) then
            begin
              LTypeData := GetTypeData(TypeInfo);
              if not Enumerations.TryGetValue(LTypeData.CompType^, LStrings) then
              begin
                LStrings := TStringList.Create;
                with TRttiEnumerationType(TRttiSetType(GetType()).ElementType) do
                begin
                  for i := MinValue to MaxValue do
                  begin
                    LStrings.Add(GetEnumName(Handle, i));
                  end;
                end;
                Enumerations.Add(LTypeData.CompType^, LStrings);
              end;
              AResult := TValue.From<TStrings>(LStrings);
              Result := True;
            end;
          end;
        end;
      end;
      tkUString:
      begin
        case ATypeInfo.Kind of
          tkEnumeration:
          begin
            if ATypeInfo = System.TypeInfo(Boolean) then
            begin
              AResult := TValue.FromBoolean(StrToBoolDef(AsString, False));
              Result := True;
            end else
            begin
              AResult := TValue.FromOrdinal(ATypeInfo, GetEnumValue(ATypeInfo, AsString));
              Result := True;
            end;
          end;
          tkInteger, tkChar, tkInt64:
          begin
            AResult := TValue.FromOrdinal(ATypeInfo, StrToIntDef(AsString, 0));
            Result := True;
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
          tkClass:
          begin
            if IsTypeCovariantTo(TypeInfo, ATypeInfo) then
            begin
              AResult := TValue.From(GetReferenceToRawData, ATypeInfo);
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
      tkInterface:
      begin
        case ATypeInfo.Kind of
          tkInterface:
          begin
            if IsTypeCovariantTo(TypeInfo, ATypeInfo) then
            begin
              AResult := TValue.From(GetReferenceToRawData, ATypeInfo);
              Result := True;
            end else if TryGetRttiType(TypeInfo, LType) and (ATypeInfo.Name = 'IList')
              and LType.IsGenericTypeOf('IList') and LType.TryGetMethod('AsList', LMethod) then
            begin
              LInterface := LMethod.Invoke(Self, []).AsInterface;
              TValue.Make(@LInterface, ATypeInfo, AResult);
              Result := True;
            end;
          end;
          tkClass:
          begin
            Result := TValue.From<TObject>(Self.AsInterface as TObject).TryConvert(ATypeInfo, AResult);
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
end;

initialization
  Enumerations := TObjectDictionary<PTypeInfo, TStrings>.Create([doOwnsValues]);

finalization
  Enumerations.Free;

end.
