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

type
  ILogging = interface;

  TGenericsLogging = record
  private
    FLogging: ILogging;
  public
    procedure LogValue<T>(AValue: T; const AName: string = '');
  end;

  ILogging = interface
    ['{F518C1FC-9BB2-4614-AAD4-BBA64B7DE70B}']
    procedure EnterMethod(const AName: string); overload;
    procedure EnterMethod(const AName: string; AClass: TClass); overload;
    procedure EnterMethod(const AName: string; AInstance: TObject); overload;
    procedure LeaveMethod(const AName: string); overload;
    procedure LeaveMethod(const AName: string; AClass: TClass); overload;
    procedure LeaveMethod(const AName: string; AInstance: TObject); overload;
    procedure LogMessage(const AMessage: string);
    procedure LogValue(AValue: Variant; const AName: string = '');  // to keep it simple for now

    function Generics: TGenericsLogging;
  end;

  ILoggingRegistration = interface
    ['{CB9F53B0-0485-4D15-916E-D3F1ECC7B03A}']
    procedure RegisterLogging(ALogging: ILogging);
    procedure UnregisterLogging(ALogging: ILogging);
  end;

  TBaseLogging = class abstract(TInterfacedObject, ILogging)
  public
    procedure EnterMethod(const AName: string); overload; virtual; abstract;
    procedure EnterMethod(const AName: string; AClass: TClass); overload; virtual; abstract;
    procedure EnterMethod(const AName: string; AInstance: TObject); overload; virtual; abstract;
    procedure LeaveMethod(const AName: string); overload; virtual; abstract;
    procedure LeaveMethod(const AName: string; AClass: TClass); overload; virtual; abstract;
    procedure LeaveMethod(const AName: string; AInstance: TObject); overload; virtual; abstract;
    procedure LogMessage(const AMessage: string); virtual; abstract;
    procedure LogValue(AValue: Variant; const AName: string = ''); virtual; abstract; // to keep it simple for now

    function Generics: TGenericsLogging;
  end;

function Logging: ILogging;
function LoggingRegistration: ILoggingRegistration;

implementation

uses
  DSharp.Collections,
{$IFDEF DEBUG}
//  DSharp.Core.Logging.Console,
//  DSharp.Core.Logging.SmartInspect,
{$ENDIF}
  Rtti,
  SysUtils;

var
  GLogging: ILogging;

type
  TLogging = class(TBaseLogging, ILoggingRegistration)
  private
    FLoggings: TList<ILogging>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterMethod(const AName: string); overload; override;
    procedure EnterMethod(const AName: string; AClass: TClass); overload; override;
    procedure EnterMethod(const AName: string; AInstance: TObject); overload; override;
    procedure LeaveMethod(const AName: string); overload; override;
    procedure LeaveMethod(const AName: string; AClass: TClass); overload; override;
    procedure LeaveMethod(const AName: string; AInstance: TObject); overload; override;
    procedure LogMessage(const AMessage: string); override;
    procedure LogValue(AValue: Variant; const AName: string = ''); override;

    procedure RegisterLogging(ALogging: ILogging);
    procedure UnregisterLogging(ALogging: ILogging);
  end;

function Logging: ILogging;
begin
  if GLogging = nil then
    GLogging := TLogging.Create;
  Result := GLogging;
end;

function LoggingRegistration: ILoggingRegistration;
begin
  Supports(Logging(), ILoggingRegistration, Result);
end;

{ TBaseLogging }

function TBaseLogging.Generics: TGenericsLogging;
begin
  Result.FLogging := Self;
end;

{ TLogging }

constructor TLogging.Create;
begin
  FLoggings := TList<ILogging>.Create();
end;

destructor TLogging.Destroy;
begin
  FLoggings.Free();
  inherited;
end;

procedure TLogging.EnterMethod(const AName: string);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.EnterMethod(AName);
end;

procedure TLogging.EnterMethod(const AName: string; AClass: TClass);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.EnterMethod(AName, AClass);
end;

procedure TLogging.EnterMethod(const AName: string; AInstance: TObject);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.EnterMethod(AName, AInstance);
end;

procedure TLogging.LeaveMethod(const AName: string);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.LeaveMethod(AName);
end;

procedure TLogging.LeaveMethod(const AName: string; AClass: TClass);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.LeaveMethod(AName, AClass);
end;

procedure TLogging.LeaveMethod(const AName: string; AInstance: TObject);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.LeaveMethod(AName, AInstance);
end;

procedure TLogging.LogMessage(const AMessage: string);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.LogMessage(AMessage);
end;

procedure TLogging.LogValue(AValue: Variant; const AName: string);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.LogValue(AValue, AName);
end;

procedure TLogging.RegisterLogging(ALogging: ILogging);
begin
  FLoggings.Add(ALogging);
end;

procedure TLogging.UnregisterLogging(ALogging: ILogging);
begin
  FLoggings.Remove(ALogging);
end;

{ TGenericsLogging }

procedure TGenericsLogging.LogValue<T>(AValue: T; const AName: string);
begin
  (FLogging as TBaseLogging).LogValue(TValue.From<T>(AValue).AsVariant, AName);
end;

end.

