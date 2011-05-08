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

unit System.Data.Conversion.Default;

interface

uses
  Generics.Collections,
  System.Data.Conversion,
  SysUtils,
  TypInfo;

type
  TDefaultConverter = class(TInterfacedObject, IValueConverter)
  private
    FSourceType: PTypeInfo;
    FTargetType: PTypeInfo;
  public
    constructor Create(ASourceType, ATargetType: PTypeInfo);
    function Convert(Value: TValue): TValue;
    function ConvertBack(Value: TValue): TValue;
  end;

type
  TValueHelper = record helper for TValue
  public
    function IsFloat: Boolean;
    function IsNumeric: Boolean;
    function IsString: Boolean;

    function TryCastEx(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;
  end;

implementation

{ TValueHelper }

function TValueHelper.IsFloat: Boolean;
begin
  Result := Self.Kind = tkFloat;
end;

function TValueHelper.IsNumeric: Boolean;
const
  tkNumeric = [tkInteger, tkChar, tkWChar, tkEnumeration, tkFloat, tkInt64];
begin
  Result := Self.Kind in tkNumeric;
end;

function TValueHelper.IsString: Boolean;
begin
  Result := Self.Kind in [tkString, tkLString, tkWString, tkUString];
end;

function TValueHelper.TryCastEx(ATypeInfo: PTypeInfo;
  out AResult: TValue): Boolean;
begin
  Result := False;
  if not Result then
  begin
    case Kind of
      tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
      begin
        case ATypeInfo.Kind of
          tkUString:
          begin
            if TypeInfo = System.TypeInfo(Boolean) then
            begin
              AResult := TValue.From<string>(BoolToStr(AsBoolean, True));
              Result := True;
            end
            else
            begin
              AResult := TValue.From<string>(IntToStr(AsOrdinal));
              Result := True;
            end;
          end;
        end;
      end;
      tkFloat:
      begin
        case ATypeInfo.Kind of
          tkUString:
          begin
            if TypeInfo = System.TypeInfo(TDate) then
            begin
              AResult := TValue.From<string>(DateToStr(AsExtended));
              Result := True;
            end
            else
            if TypeInfo = System.TypeInfo(TDateTime) then
            begin
              AResult := TValue.From<string>(DateTimeToStr(AsExtended));
              Result := True;
            end
            else
            if TypeInfo = System.TypeInfo(TTime) then
            begin
              AResult := TValue.From<string>(TimeToStr(AsExtended));
              Result := True;
            end
            else
            begin
              AResult := TValue.From<string>(FloatToStr(AsExtended));
              Result := True;
            end;
          end;
        end;
      end;
      tkUString:
      begin
        case ATypeInfo.Kind of
          tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
          begin
            if TypeInfo = System.TypeInfo(Boolean) then
            begin
              AResult := TValue.From<Boolean>(StrToBoolDef(AsString, False));
              Result := True;
            end
            else
            begin
              AResult := TValue.FromOrdinal(ATypeInfo, StrToIntDef(AsString, 0));
              Result := True;
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
              AResult := TValue.From<Extended>(StrToFloatDef(AsString, 0));
              Result := True;
            end;
          end;
        end;
      end;
    end;
  end;
  if not Result then
  begin
    Result := TryCast(ATypeInfo, AResult);
  end;
end;

{ TDefaultConverter }

constructor TDefaultConverter.Create(ASourceType, ATargetType: PTypeInfo);
begin
  FSourceType := ASourceType;
  FTargetType := ATargetType;
end;

function TDefaultConverter.Convert(Value: TValue): TValue;
begin
  Value.TryCastEx(FTargetType, Result);
end;

function TDefaultConverter.ConvertBack(Value: TValue): TValue;
begin
  Value.TryCastEx(FSourceType, Result);
end;

end.
