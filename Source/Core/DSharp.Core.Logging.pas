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
  ILogging = interface
    ['{F518C1FC-9BB2-4614-AAD4-BBA64B7DE70B}']
    procedure EnterMethod(const AName: string; AInstance: TObject = nil);
    procedure LeaveMethod(const AName: string; AInstance: TObject = nil);
    procedure LogMessage(const AMessage: string);
    procedure LogValue(AValue: Variant; const AName: string = '');  // to keep it simple for now
  end;

  ILoggingRegistration = interface
    ['{CB9F53B0-0485-4D15-916E-D3F1ECC7B03A}']
    procedure RegisterLogging(ALogging: ILogging);
    procedure UnregisterLogging(ALogging: ILogging);
  end;

function Logging: ILogging;
function LoggingRegistration: ILoggingRegistration;

implementation

uses
  Generics.Collections,
  SysUtils;

var
  GLogging: ILogging;

type
  TLogging = class(TInterfacedObject, ILogging, ILoggingRegistration)
  private
    FLoggings: TList<ILogging>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterMethod(const AName: string; AInstance: TObject = nil);
    procedure LeaveMethod(const AName: string; AInstance: TObject = nil);
    procedure LogMessage(const AMessage: string);
    procedure LogValue(AValue: Variant; const AName: string = '');

    procedure RegisterLogging(ALogging: ILogging);
    procedure UnregisterLogging(ALogging: ILogging);
  end;

function Logging: ILogging;
begin
  Result := GLogging;
end;

function LoggingRegistration: ILoggingRegistration;
begin
  Supports(GLogging, ILoggingRegistration, Result);
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

procedure TLogging.EnterMethod(const AName: string; AInstance: TObject);
var
  LLogging: ILogging;
begin
  for LLogging in FLoggings do
    LLogging.EnterMethod(AName, AInstance);
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

initialization
  GLogging := TLogging.Create;

end.
