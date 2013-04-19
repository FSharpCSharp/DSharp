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

unit DSharp.Collections.Threading;

interface

uses
  Classes,
  DSharp.Core.Threading,
  Rtti,
  SysUtils;

type
  TEnumeratorThread = class(TAbstractTaskThread, IInterface)
  private
    FProc: TProc;
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  protected
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    procedure Execute; override;
    procedure SetCurrent(const AValue); virtual; abstract;
  public
    constructor Create(const AProc: TProc);
    function MoveNext: Boolean;
    procedure Yield(const AValue); overload;
    procedure Yield(const AValue: TValue); overload; virtual; abstract;
    property Current: TValue read GetCurrentNonGeneric;
  end;

  TEnumeratorThread<T> = class(TEnumeratorThread)
  private
    FCurrent: T;
    function GetCurrent: T;
  protected
    function GetCurrentNonGeneric: TValue; override;
    procedure SetCurrent(const AValue); override;
  public
    procedure Yield(const AValue: TValue); override;
    property Current: T read GetCurrent;
  end;

implementation

{ TEnumeratorThread }

constructor TEnumeratorThread.Create(const AProc: TProc);
begin
  inherited Create;
  FProc := AProc;
end;

function TEnumeratorThread.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TEnumeratorThread._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TEnumeratorThread._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result = 0 then
    Destroy;
end;

procedure TEnumeratorThread.Execute;
begin
  inherited;
  FProc;
end;

function TEnumeratorThread.MoveNext: Boolean;
begin
  Continue;
  Result := not Finished;
end;

procedure TEnumeratorThread.Yield(const AValue);
begin
  SetCurrent(AValue);
  inherited Yield;
end;

{ TEnumeratorThread<T> }

function TEnumeratorThread<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(FCurrent);
end;

function TEnumeratorThread<T>.GetCurrent: T;
begin
  Result := FCurrent;
end;

procedure TEnumeratorThread<T>.SetCurrent(const AValue);
begin
  FCurrent := T(AValue);
end;

procedure TEnumeratorThread<T>.Yield(const AValue: TValue);
var
  LValue: T;
begin
  LValue := AValue.AsType<T>;
  inherited Yield(LValue);
end;

end.
