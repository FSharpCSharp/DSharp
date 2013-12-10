(*
  Copyright (c) 2011-2013, Stefan Glienke
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

unit DSharp.Logging;

interface

uses
  Classes,
  Generics.Collections,
  Rtti,
  SysUtils,
  TypInfo;

type
  TLogKind = (
    lkEnterMethod,
    lkLeaveMethod,
    lkMessage,
    lkWarning,
    lkError,
    lkException,
    lkValue);

  TLogEntry = record
  private
    FLogKind: TLogKind;
    FFormatText: string;
    FValues: TArray<TValue>;
    function GetValue: TValue;
  public
    constructor Create(const AFormatText: string; const AValues: array of TValue; const ALogKind: TLogKind);

    property LogKind: TLogKind read FLogKind;
    property FormatText: string read FFormatText;
    property Values: TArray<TValue> read FValues;
    property Value: TValue read GetValue;
  end;

  ILog = interface
    ['{23190A44-748D-457D-A6BF-DBD739AE325B}']
    procedure LogEntry(const ALogEntry: TLogEntry);

    procedure EnterMethod(const AName: string); overload;
    procedure EnterMethod(AValue: TValue; const AName: string); overload;
    procedure LeaveMethod(const AName: string); overload;
    procedure LeaveMethod(AValue: TValue; const AName: string); overload;
    procedure LogError(const ATitle: string); overload;
    procedure LogError(const AFormatText: string; const AValues: array of TValue); overload;
    procedure LogException(AException: Exception = nil; const ATitle: string = '');
    procedure LogMessage(const ATitle: string); overload;
    procedure LogMessage(const AFormatText: string; const AValues: array of TValue); overload;
    procedure LogValue(const AName: string; const AValue: TValue);
    procedure LogWarning(const ATitle: string); overload;
    procedure LogWarning(const AFormatText: string; const AValues: array of TValue); overload;
  end;

  TLogBaseClass = class of TLogBase;
  TLogBase = class abstract(TInterfacedObject, ILog)
  strict private
    FTypeInfo: PTypeInfo;
  strict protected
    procedure LogEntry(const ALogEntry: TLogEntry); virtual; abstract;
    function GetTypeInfoName: string; virtual;
    function GetTypeInfo: PTypeInfo; virtual;
  public
    constructor Create(); overload; virtual;
    constructor Create(const ATypeInfo: PTypeInfo); overload; virtual;
    destructor Destroy(); override;
    procedure EnterMethod(const AName: string); overload;
    procedure EnterMethod(AValue: TValue; const AName: string); overload;
    procedure LeaveMethod(const AName: string); overload;
    procedure LeaveMethod(AValue: TValue; const AName: string); overload;
    procedure LogError(const ATitle: string); overload;
    procedure LogError(const AFormatText: string; const AValues: array of TValue); overload;
    procedure LogException(AException: Exception = nil; const ATitle: string = '');
    procedure LogMessage(const ATitle: string); overload;
    procedure LogMessage(const AFormatText: string; const AValues: array of TValue); overload;
    procedure LogValue(const AName: string; const AValue: TValue); overload;
    procedure LogValue<T>(const AName: string; const AValue: T); overload;
    procedure LogWarning(const ATitle: string); overload;
    procedure LogWarning(const AFormatText: string; const AValues: array of TValue); overload;
  end;

  TTextLog = class abstract(TLogBase)
  protected
    procedure LogEntry(const ALogEntry: TLogEntry); override;
    procedure WriteLine(const Text: string); virtual; abstract;
  end;

  TStringsLog = class(TTextLog)
  private
    FStrings: TStrings;
  protected
    procedure WriteLine(const Text: string); override;
  public
    constructor Create(AStrings: TStrings);
  end;

  LogManager = record
  private
    class var
      FGetLog: TFunc<PTypeInfo, ILog>;
      FNullLog: ILog;
  public
    class constructor Create;
    class property GetLog: TFunc<PTypeInfo, ILog> read FGetLog write FGetLog;

    type
      TNullLog = class(TLogBase)
      protected
        procedure LogEntry(const ALogEntry: TLogEntry); override;
      end;
  end;

function Logging: ILog; overload;
function Logging(ATypeInfo: PTypeInfo): ILog; overload;
procedure RegisterLogging(ALog: ILog);
procedure UnregisterLogging(ALog: ILog);

implementation

uses
  DSharp.Core.Reflection;

type
  TLogProxy = class(TLogBase)
  private
    FLogs: TList<ILog>;
  protected
    procedure LogEntry(const ALogEntry: TLogEntry); override;
    property Logs: TList<ILog> read FLogs;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  GLog: ILog;

resourcestring
  REnterMethod = 'Enter: ';
  RLeaveMethod = 'Leave: ';

function Format(const Format: string; const Args: array of TValue): string;
begin
  Result := SysUtils.Format(Format, TValue.ToVarRecs(Args));
end;

function Logging: ILog;
begin
  Result := Logging(nil);
end;

function Logging(ATypeInfo: PTypeInfo): ILog;
begin
  if not Assigned(GLog) then
    GLog := TLogProxy.Create;
  Result := GLog;
end;

procedure RegisterLogging(ALog: ILog);
begin
  (Logging as TLogProxy).Logs.Add(ALog);
end;

procedure UnregisterLogging(ALog: ILog);
begin
  (Logging as TLogProxy).Logs.Remove(ALog);
end;

{ TLogBase }

procedure TLogBase.EnterMethod(const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, [], lkEnterMethod));
end;

constructor TLogBase.Create(const ATypeInfo: PTypeInfo);
begin
  inherited Create();
  FTypeInfo := ATypeInfo;
//  LogMessage('%s.Create(Category=%s)', [QualifiedClassName, GetTypeInfoName()]); // ##jwp for tracking down a memory leak
end;

constructor TLogBase.Create();
begin
  Create(nil);
end;

destructor TLogBase.Destroy();
begin
//  LogMessage('%s.Destroy(Category=%s)', [QualifiedClassName, GetTypeInfoName()]); // ##jwp for tracking down a memory leak
  inherited;
end;

procedure TLogBase.EnterMethod(AValue: TValue; const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, [AValue], lkEnterMethod));
end;

function TLogBase.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

procedure TLogBase.LeaveMethod(const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, [], lkLeaveMethod));
end;

procedure TLogBase.LeaveMethod(AValue: TValue; const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, [AValue], lkLeaveMethod));
end;

procedure TLogBase.LogError(const ATitle: string);
begin
  LogError(ATitle, []);
end;

procedure TLogBase.LogError(const AFormatText: string; const AValues: array of TValue);
begin
  LogEntry(TLogEntry.Create(AFormatText, AValues, lkError));
end;

procedure TLogBase.LogException(AException: Exception; const ATitle: string);
begin
  if not Assigned(AException) then
  begin
    AException := ExceptObject as Exception;
  end;
  LogEntry(TLogEntry.Create(ATitle, [AException], lkException));
end;

procedure TLogBase.LogMessage(const ATitle: string);
begin
  LogMessage(ATitle, []);
end;

procedure TLogBase.LogMessage(const AFormatText: string; const AValues: array of TValue);
begin
  LogEntry(TLogEntry.Create(AFormatText, AValues, lkMessage));
end;

procedure TLogBase.LogValue(const AName: string; const AValue: TValue);
begin
  LogEntry(TLogEntry.Create(AName, [AValue], lkValue));
end;

procedure TLogBase.LogValue<T>(const AName: string; const AValue: T);
begin
  LogEntry(TLogEntry.Create(AName, [TValue.From<T>(AValue)], lkValue));
end;

procedure TLogBase.LogWarning(const ATitle: string);
begin
  LogWarning(ATitle, []);
end;

procedure TLogBase.LogWarning(const AFormatText: string; const AValues: array of TValue);
begin
  LogEntry(TLogEntry.Create(AFormatText, AValues, lkWarning));
end;

function TLogBase.GetTypeInfoName: string;
var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := GetTypeInfo();
  if Assigned(LTypeInfo) then
{$IF CompilerVersion > 22} {-o##jwp -cCompatibility : XE2 and higher; before that, use the Name property}
    Result := LTypeInfo.NameFld.ToString()
{$ELSE}
    Result := string(LTypeInfo.Name)
{$IFEND}
  else
    Result := '';
end;

{ TLogProxy }

constructor TLogProxy.Create;
begin
  FLogs := TList<ILog>.Create();
end;

destructor TLogProxy.Destroy;
begin
  FLogs.Free();
  FLogs := nil;
  inherited;
end;

procedure TLogProxy.LogEntry(const ALogEntry: TLogEntry);
var
  LLog: ILog;
begin
  if Assigned(FLogs) then
    for LLog in FLogs do
      LLog.LogEntry(ALogEntry);
end;

{ TLogEntry }

constructor TLogEntry.Create(const AFormatText: string; const AValues: array of TValue; const ALogKind: TLogKind);
begin
  FLogKind := ALogKind;
  FFormatText := AFormatText;
  FValues := TArrayHelper.Copy<TValue>(AValues);
end;

{ TTextLog }

procedure TTextLog.LogEntry(const ALogEntry: TLogEntry);
var
  LMessage: string;
  LValue: TValue;
begin
  {TODO -o##jwp -cEnhance : Find a smart way to insert a kind of Category here from GetTypeInfoName() }
  LValue := ALogEntry.Value;
  case ALogEntry.LogKind of
    lkEnterMethod:
    begin
      LMessage := REnterMethod;
      if not LValue.IsEmpty then
        LMessage := LMessage + UTF8ToString(LValue.TypeInfo.Name) + '.';
      LMessage := LMessage + ALogEntry.FormatText;
    end;
    lkLeaveMethod:
    begin
      LMessage := RLeaveMethod;
      if not LValue.IsEmpty then
        LMessage := LMessage + UTF8ToString(LValue.TypeInfo.Name) + '.';
      LMessage := LMessage + ALogEntry.FormatText;
    end;
    lkMessage:
    begin
      LMessage := Format('INFO: ' + ALogEntry.FormatText, ALogEntry.Values);
    end;
    lkWarning:
    begin
      LMessage := Format('WARN: ' + ALogEntry.FormatText, ALogEntry.Values);
    end;
    lkError:
    begin
      LMessage := Format('ERROR: ' + ALogEntry.FormatText, ALogEntry.Values);
    end;
    lkException:
    begin
      if ALogEntry.FormatText <> '' then
        LMessage := ALogEntry.FormatText + ': ';
      LMessage := LMessage + LValue.AsType<Exception>.ToString;
    end;
    lkValue:
    begin
      if ALogEntry.FormatText <> '' then
        LMessage := ALogEntry.FormatText + ': ';
      LMessage := LMessage + TValue.ToString(LValue);
    end;
  end;

  WriteLine(LMessage);
end;

{ TStringsLog }

constructor TStringsLog.Create(AStrings: TStrings);
begin
  FStrings := AStrings;
end;

procedure TStringsLog.WriteLine(const Text: string);
begin
  FStrings.Add(Text);
end;

function TLogEntry.GetValue: TValue;
begin
  if Length(FValues) > 0 then
    Result := FValues[0];
end;

{ LogManager }

class constructor LogManager.Create;
begin
  FNullLog := TNullLog.Create();
  FGetLog :=
    function(TypeInfo: PTypeInfo): ILog
    begin
      Result := FNullLog;
    end;
end;

{ LogManager.TNullLog }

procedure LogManager.TNullLog.LogEntry(const ALogEntry: TLogEntry);
begin
  // nothing to do
end;

end.
