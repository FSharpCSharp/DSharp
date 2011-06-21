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

unit DSharp.Core.Logging.SmartInspect;

interface

uses
  DSharp.Core.Logging;

type
  TSmartInspectLogging = class abstract(TBaseLogging)
  public
    procedure EnterMethod(const AName: string); overload; override;
    procedure EnterMethod(const AName: string; AClass: TClass); overload; override;
    procedure EnterMethod(const AName: string; AInstance: TObject); overload; override;
    procedure LeaveMethod(const AName: string); overload; override;
    procedure LeaveMethod(const AName: string; AClass: TClass); overload; override;
    procedure LeaveMethod(const AName: string; AInstance: TObject); overload; override;
    procedure LogMessage(const AMessage: string); overload;
    procedure LogValue(AValue: Variant; const AName: string = '');overload;
  end;


implementation

uses
  SiAuto,
  Variants;

{ TSmartInspectLogging }

procedure TSmartInspectLogging.EnterMethod(const AName: string);
begin
  SiMain.EnterMethod(AName);
end;

procedure TSmartInspectLogging.EnterMethod(const AName: string;
  AInstance: TObject);
begin
  SiMain.EnterMethod(AInstance, AName);
end;

procedure TSmartInspectLogging.EnterMethod(const AName: string; AClass: TClass);
begin
  if Assigned(AClass) then
    SiMain.EnterMethod(AClass.ClassName + '.' + AName)
  else
    SiMain.EnterMethod(AName);
end;

procedure TSmartInspectLogging.LeaveMethod(const AName: string;
  AInstance: TObject);
begin
  SiMain.LeaveMethod(AInstance, AName);
end;

procedure TSmartInspectLogging.LeaveMethod(const AName: string; AClass: TClass);
begin
  if Assigned(AClass) then
    SiMain.LeaveMethod(AClass.ClassName + '.' + AName)
  else
    SiMain.LeaveMethod(AName);
end;

procedure TSmartInspectLogging.LeaveMethod(const AName: string);
begin
  SiMain.LeaveMethod(AName);
end;

procedure TSmartInspectLogging.LogMessage(const AMessage: string);
begin
  SiMain.LogMessage(AMessage);
end;

procedure TSmartInspectLogging.LogValue(AValue: Variant; const AName: string);
begin
  case VarType(AValue) of
    varSmallint: SiMain.LogSmallint(AName, AValue);
    varInteger: SiMain.LogInteger(AName, AValue);
    varSingle: SiMain.LogSingle(AName, AValue);
    varDouble: SiMain.LogDouble(AName, AValue);
    varCurrency: SiMain.LogCurrency(AName, AValue);
    varDate: SiMain.LogDateTime(AName, AValue);
    varBoolean: SiMain.LogBoolean(AName, AValue);
    varShortInt: SiMain.LogShortint(AName, AValue);
    varByte: SiMain.LogByte(AName, AValue);
    varWord: SiMain.LogWord(AName, AValue);
    varLongWord: SiMain.LogCardinal(AName, AValue);
    varInt64: SiMain.LogInt64(AName, AValue);
//    varUInt64:
    varString, varUString: SiMain.LogString(AName, AValue);
  end;
end;

initialization
  LoggingRegistration.RegisterLogging(TSmartInspectLogging.Create);

end.
