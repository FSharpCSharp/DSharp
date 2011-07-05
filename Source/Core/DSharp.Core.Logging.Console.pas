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

unit DSharp.Core.Logging.Console;

interface

implementation

uses
  DSharp.Core.Logging,
  Rtti,
  StrUtils,
  Windows;

resourcestring
  REnterMethod = 'Enter: ';
  RLeaveMethod = 'Leave: ';

type
  TConsoleLogging = class(TBaseLogging)
  public
    procedure EnterMethod(const AName: string); overload; override;
    procedure EnterMethod(const AName: string; AClass: TClass); overload; override;
    procedure EnterMethod(const AName: string; AInstance: TObject); overload; override;
    procedure LeaveMethod(const AName: string); overload; override;
    procedure LeaveMethod(const AName: string; AClass: TClass); overload; override;
    procedure LeaveMethod(const AName: string; AInstance: TObject = nil); overload; override;
    procedure LogMessage(const AMessage: string); override;
    procedure LogValue(AValue: Variant; const AName: string = ''); override;
  end;

{ TConsoleLogging }

procedure TConsoleLogging.EnterMethod(const AName: string);
begin
  Writeln(REnterMethod + AName);
end;

procedure TConsoleLogging.EnterMethod(const AName: string; AClass: TClass);
begin
  Writeln(REnterMethod + AClass.ClassName + '.' + AName);
end;

procedure TConsoleLogging.EnterMethod(const AName: string; AInstance: TObject);
begin
  Writeln(REnterMethod + AInstance.ClassName + '.' + AName);
end;

procedure TConsoleLogging.LeaveMethod(const AName: string);
begin
  Writeln(RLeaveMethod + AName);
end;

procedure TConsoleLogging.LeaveMethod(const AName: string; AClass: TClass);
begin
  Writeln(RLeaveMethod + AClass.ClassName + '.' + AName);
end;

procedure TConsoleLogging.LeaveMethod(const AName: string; AInstance: TObject);
begin
  Writeln(RLeaveMethod + AInstance.ClassName + '.' + AName);
end;

procedure TConsoleLogging.LogMessage(const AMessage: string);
begin
  Writeln(AMessage);
end;

procedure TConsoleLogging.LogValue(AValue: Variant; const AName: string);
begin
  if AName <> '' then
    Write(AName + ': ');
  Writeln(TValue.FromVariant(AValue).ToString);
end;

initialization
  AllocConsole;
  LoggingRegistration.RegisterLogging(TConsoleLogging.Create);

end.