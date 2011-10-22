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

unit DSharp.Logging.CodeSite.Helper;

interface

uses
  CodeSiteLogging,
  Rtti;

type
  TCodeSiteLoggerHelper = class helper for TCodeSiteLogger
  public
    procedure Send(const Msg: string; const Value: TValue); overload;
    procedure Send(const Msg: string; const Value: Variant); overload;
  end;


implementation

uses
  DSharp.Core.Reflection,
  TypInfo,
  Variants;

{ TCodeSiteLoggerHelper }

procedure TCodeSiteLoggerHelper.Send(const Msg: string; const Value: TValue);
begin
  case Value.Kind of
    tkInteger:
    begin
      if Value.IsInteger then
      begin
        Send(Msg, Value.AsInteger);
      end
      else
      if Value.IsCardinal then
      begin
        Send(Msg, Value.AsCardinal);
      end
      else
      if Value.IsSmallInt then
      begin
        Send(Msg, Value.AsSmallInt);
      end
      else
      if Value.IsWord then
      begin
        Send(Msg, Value.AsWord);
      end
      else
      if Value.IsShortInt then
      begin
        Send(Msg, Value.AsShortInt);
      end
      else
      if Value.IsByte then
      begin
        Send(Msg, Value.AsByte);
      end;
    end;
    tkEnumeration:
    begin
      if Value.TypeInfo = TypeInfo(Boolean) then
      begin
        Send(Msg, Value.AsBoolean);
      end;
    end;
    tkFloat:
    begin
      if Value.TypeInfo = TypeInfo(TDate) then
      begin
        SendDateTime(Msg, Value.AsType<Double>);
      end
      else
      if Value.TypeInfo = TypeInfo(TDateTime) then
      begin
        SendDateTime(Msg, Value.AsType<Double>);
      end
      else
      if Value.TypeInfo = TypeInfo(TTime) then
      begin
        SendDateTime(Msg, Value.AsType<Double>);
      end
      else
      begin
        case Value.TypeData.FloatType of
          ftSingle: Send(Msg, Value.AsType<Single>);
          ftDouble: Send(Msg, Value.AsType<Double>);
          ftCurr: SendCurrency(Msg, Value.AsCurrency);
        else
          Send(Msg, Value.AsExtended);
        end;
      end;
    end;
    tkClass: Send(Msg, Value.AsObject);
    tkVariant: Send(Msg, Value.AsVariant);
    tkInt64: Send(Msg, Value.AsInt64);
  else
    Send(Msg, Value.ToString);
  end;

end;

procedure TCodeSiteLoggerHelper.Send(const Msg: string; const Value: Variant);
var
  LVarType: Word;
begin
  LVarType := VarType(Value);
  case LVarType of
    varNull: Send(Msg, '(null)');
    varSmallint: Send(Msg, SmallInt(Value));
    varInteger: Send(Msg, Integer(Value));
    varSingle: Send(Msg, Single(Value));
    varDouble: Send(Msg, Double(Value));
    varCurrency: Send(Msg, Currency(Value));
    varDate: SendDateTime(Msg, Value);
    varBoolean: Send(Msg, Boolean(Value));
    varShortInt: Send(Msg, ShortInt(Value));
    varByte: Send(Msg, Byte(Value));
    varWord: Send(Msg, Word(Value));
    varLongWord: Send(Msg, LongWord(Value));
    varInt64: Send(Msg, Int64(Value));
//    varUInt64: LogUInt64(ALevel, AName, AValue, AIncludeHex);
    varString, varUString: Send(Msg, VarToStr(Value));
  else
    Send(Msg, VarToStr(Value));
  end;
end;

end.