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

unit DSharp.Core.Logging;

interface

uses
  DSharp.Collections,
  Rtti,
  SysUtils;

type
  TLogKind = (lkEnterMethod, lkLeaveMethod, lkMessage, lkException, lkValue);

  TLogEntry = record
    LogKind: TLogKind;
    Name: string;
    Value: TValue;
    constructor Create(const AName: string; const AValue: TValue;
      const ALogKind: TLogKind);
  end;

  TBaseLogging = class abstract
  protected
    procedure LogEntry(const ALogEntry: TLogEntry); virtual; abstract;
  public
    procedure EnterMethod(const AName: string); overload;
    procedure EnterMethod(const AClass: TClass; const AName: string); overload;
    procedure EnterMethod(const AInstance: TObject; const AName: string); overload;
    procedure LeaveMethod(const AName: string); overload;
    procedure LeaveMethod(const AClass: TClass; const AName: string); overload;
    procedure LeaveMethod(const AInstance: TObject; const AName: string); overload;
    procedure LogException(const AException: Exception; const ATitle: string = '');
    procedure LogMessage(const AMessage: string);
    procedure LogValue<T>(const AName: string; const AValue: T);
  end;

function Logging: TBaseLogging;
procedure RegisterLogging(ALogging: TBaseLogging);
procedure UnregisterLogging(ALogging: TBaseLogging);

implementation

{$IFDEF DEBUG}
uses
//  DSharp.Core.Logging.Console,
  DSharp.Core.Logging.SmartInspect;
{$ENDIF}

type
  TLogging = class(TBaseLogging)
  private
    FLoggings: TList<TBaseLogging>;
  protected
    procedure LogEntry(const ALogEntry: TLogEntry); override;
    property Loggings: TList<TBaseLogging> read FLoggings;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  GLogging: TLogging;

function Logging: TBaseLogging;
begin
  if not Assigned(GLogging) then
    GLogging := TLogging.Create;
  Result := GLogging;
end;

procedure RegisterLogging(ALogging: TBaseLogging);
begin
  TLogging(Logging).Loggings.Add(ALogging);
end;

procedure UnregisterLogging(ALogging: TBaseLogging);
begin
  TLogging(Logging).Loggings.Remove(ALogging);
end;

{ TBaseLogging }

procedure TBaseLogging.EnterMethod(const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, TValue.Empty, lkEnterMethod));
end;

procedure TBaseLogging.EnterMethod(const AClass: TClass; const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, TValue.From<TClass>(AClass), lkEnterMethod));
end;

procedure TBaseLogging.EnterMethod(const AInstance: TObject; const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, TValue.From<TObject>(AInstance), lkEnterMethod));
end;

procedure TBaseLogging.LeaveMethod(const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, TValue.Empty, lkLeaveMethod));
end;

procedure TBaseLogging.LeaveMethod(const AClass: TClass; const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, TValue.From<TClass>(AClass), lkLeaveMethod));
end;

procedure TBaseLogging.LeaveMethod(const AInstance: TObject; const AName: string);
begin
  LogEntry(TLogEntry.Create(AName, TValue.From<TObject>(AInstance), lkLeaveMethod));
end;

procedure TBaseLogging.LogException(const AException: Exception; const ATitle: string);
begin
  LogEntry(TLogEntry.Create(ATitle, TValue.From<Exception>(AException), lkException));
end;

procedure TBaseLogging.LogMessage(const AMessage: string);
begin
  LogEntry(TLogEntry.Create(AMessage, TValue.Empty, lkMessage));
end;

procedure TBaseLogging.LogValue<T>(const AName: string; const AValue: T);
begin
  LogEntry(TLogEntry.Create(AName, TValue.From<T>(AValue), lkValue));
end;

{ TLogging }

constructor TLogging.Create;
begin
  FLoggings := TObjectList<TBaseLogging>.Create();
end;

destructor TLogging.Destroy;
begin
  FLoggings.Free();
  inherited;
end;

procedure TLogging.LogEntry(const ALogEntry: TLogEntry);
var
  LLogging: TBaseLogging;
begin
  for LLogging in FLoggings do
    LLogging.LogEntry(ALogEntry);
end;

{ TLogEntry }

constructor TLogEntry.Create(const AName: string; const AValue: TValue;
  const ALogKind: TLogKind);
begin
  Name := AName;
  Value := AValue;
  LogKind := ALogKind;
end;

initialization

finalization
  FreeAndNil(GLogging);

end.

