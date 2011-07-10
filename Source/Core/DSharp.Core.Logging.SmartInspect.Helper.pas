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

unit DSharp.Core.Logging.SmartInspect.Helper;

interface

uses
  Rtti,
  SmartInspect;

type
  TSiSessionHelper = class helper for TSiSession
  public
    procedure LogValue(const AName: UnicodeString;
      const AValue: TValue); overload;
    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: TValue); overload;
//    procedure LogValue(const AName: UnicodeString; const AValue: TValue
//      const AIncludeHex: Boolean); overload;
    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: TValue; const AIncludeHex: Boolean); overload;

    procedure LogValue<T>(const AName: UnicodeString;
      const AValue: T); overload;
    procedure LogValue<T>(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: T); overload;
    procedure LogValue<T>(const AName: UnicodeString; const AValue: T;
      const AIncludeHex: Boolean); overload;
    procedure LogValue<T>(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: T; const AIncludeHex: Boolean); overload;

    procedure LogVariant(const AName: UnicodeString;
      const AValue: Variant); overload;
    procedure LogVariant(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Variant); overload;
    procedure LogVariant(const AName: UnicodeString; const AValue: Variant;
      const AIncludeHex: Boolean); overload;
    procedure LogVariant(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Variant; const AIncludeHex: Boolean); overload;
  end;

implementation

uses
  TypInfo,
  Variants;

{ TSiSessionHelper }

procedure TSiSessionHelper.LogValue(const AName: UnicodeString;
  const AValue: TValue);
begin
  LogValue(Parent.DefaultLevel, AName, AValue);
end;

procedure TSiSessionHelper.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: TValue);
begin
  LogValue(ALevel, AName, AValue, False);
end;

procedure TSiSessionHelper.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: TValue; const AIncludeHex: Boolean);
begin
  case AValue.Kind of
    tkInteger:
    begin
//      case LValue.TypeData.
      LogInteger(ALevel, AName, AValue.AsInteger, AIncludeHex);
    end;
    tkEnumeration:
    begin
      if AValue.TypeInfo = TypeInfo(Boolean) then
      begin
        LogBoolean(ALevel, AName, AValue.AsBoolean);
      end;
    end;
    tkFloat:
    begin
      if AValue.TypeInfo = TypeInfo(TDate) then
      begin
        LogDateTime(ALevel, AName, AValue.AsType<Double>);
      end
      else
      if AValue.TypeInfo = TypeInfo(TDateTime) then
      begin
        LogDateTime(ALevel, AName, AValue.AsType<Double>);
      end
      else
      if AValue.TypeInfo = TypeInfo(TTime) then
      begin
        LogDateTime(ALevel, AName, AValue.AsType<Double>);
      end
      else
      begin
        case AValue.TypeData.FloatType of
          ftSingle: LogSingle(ALevel, AName, AValue.AsType<Single>);
          ftDouble: LogDouble(ALevel, AName, AValue.AsType<Double>);
          ftCurr: LogCurrency(ALevel, AName, AValue.AsCurrency);
        else
          LogExtended(ALevel, AName, AValue.AsExtended);
        end;
      end;
    end;
    tkClass: LogObject(ALevel, AName, AValue.AsObject);
    tkVariant: LogVariant(ALevel, AName, AValue.AsVariant, AIncludeHex);
    tkInt64: LogInt64(ALevel, AName, AValue.AsInt64, AIncludeHex);
  else
    LogString(ALevel, AName, AValue.ToString);
  end;
end;

procedure TSiSessionHelper.LogValue<T>(const AName: UnicodeString;
  const AValue: T);
begin
  LogValue<T>(Parent.DefaultLevel, AName, AValue);
end;

procedure TSiSessionHelper.LogValue<T>(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: T);
begin
  LogValue<T>(ALevel, AName, AValue, False);
end;

procedure TSiSessionHelper.LogValue<T>(const AName: UnicodeString;
  const AValue: T; const AIncludeHex: Boolean);
begin
  LogValue<T>(Parent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSessionHelper.LogValue<T>(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: T; const AIncludeHex: Boolean);
begin
  LogValue(ALevel, AName, TValue.From<T>(AValue), AIncludeHex);
end;

procedure TSiSessionHelper.LogVariant(const AName: UnicodeString;
  const AValue: Variant);
begin
  LogVariant(Parent.DefaultLevel, AName, AValue);
end;

procedure TSiSessionHelper.LogVariant(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Variant);
begin
  LogVariant(ALevel, AName, AValue, False);
end;

procedure TSiSessionHelper.LogVariant(const AName: UnicodeString;
  const AValue: Variant; const AIncludeHex: Boolean);
begin
  LogVariant(Parent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSessionHelper.LogVariant(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Variant;
  const AIncludeHex: Boolean);
var
  LVarType: Word;
begin
  LVarType := VarType(AValue);
  case LVarType of
    varNull: LogString(ALevel, AName, '(null)');
    varSmallint: LogSmallint(ALevel, AName, AValue, AIncludeHex);
    varInteger: LogInteger(ALevel, AName, AValue, AIncludeHex);
    varSingle: LogSingle(ALevel, AName, AValue);
    varDouble: LogDouble(ALevel, AName, AValue);
    varCurrency: LogCurrency(ALevel, AName, AValue);
    varDate: LogDateTime(ALevel, AName, AValue);
    varBoolean: LogBoolean(ALevel, AName, AValue);
    varShortInt: LogShortint(ALevel, AName, AValue, AIncludeHex);
    varByte: LogByte(ALevel, AName, AValue, AIncludeHex);
    varWord: LogWord(ALevel, AName, AValue, AIncludeHex);
    varLongWord: LogCardinal(ALevel, AName, AValue, AIncludeHex);
    varInt64: LogInt64(ALevel, AName, AValue, AIncludeHex);
//    varUInt64: LogUInt64(ALevel, AName, AValue, AIncludeHex);
    varString, varUString: LogString(ALevel, AName, AValue);
  else
    LogString(ALevel, AName, VarToStr(AValue));
  end;
end;

end.
