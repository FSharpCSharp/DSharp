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
  Spring.Collections,
  Rtti,
  SyncObjs,
  SysUtils;

type
  TWorkerThread = class(TThread)
  private
    fProc: TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(const proc: TProc);
  end;

  TIteratorThread = class(TInterfacedObject, IEnumerator)
  private
  private
    fProc: TProc;
    fThread: TWorkerThread;
    fYield: TEvent;
    fMoveNext: TEvent;
    class threadvar fCurrentThread: TIteratorThread;
    class function GetCurrentThread: TIteratorThread; static;
  protected
    procedure Execute;
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
    procedure SetCurrent(const value); virtual; abstract;
    procedure Yield(const value: TValue); overload; virtual; abstract;
  public
    constructor Create(const proc: TProc);
    destructor Destroy; override;

    function MoveNext: Boolean;
    procedure Reset;

    class procedure Yield(const value); overload;

    property Current: TValue read GetCurrentNonGeneric;
    class property CurrentThread: TIteratorThread read GetCurrentThread;
  end;

  TIteratorThread<T> = class(TIteratorThread, IEnumerator<T>)
  private
    fCurrent: T;
  protected
    function GetCurrentNonGeneric: TValue; override;
    function GetCurrent: T;
    procedure SetCurrent(const value); override;
    procedure Yield(const value: TValue); override;
  public
    property Current: T read GetCurrent;
  end;

implementation

{ TWorkerThread }

constructor TWorkerThread.Create(const proc: TProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  fProc := proc;
end;

procedure TWorkerThread.Execute;
begin
  fProc();
end;

{ TIteratorThread }

constructor TIteratorThread.Create(const proc: TProc);
begin
  inherited Create;

  fProc := proc;
  fThread := TWorkerThread.Create(Execute);
  fYield := TEvent.Create(nil, False, False, '');
  fMoveNext := TEvent.Create(nil, False, False, '');
end;

destructor TIteratorThread.Destroy;
begin
  fThread.Terminate;
  fMoveNext.SetEvent;

  fThread.Free;
  fMoveNext.Free;
  fYield.Free;

  inherited;
end;

procedure TIteratorThread.Execute;
begin
  try
    fCurrentThread := Self;
    fProc;
  finally
    fThread.Terminate;
    fYield.SetEvent;
  end;
end;

class function TIteratorThread.GetCurrentThread: TIteratorThread;
begin
  Result := fCurrentThread;
end;

function TIteratorThread.MoveNext: Boolean;
begin
  if fThread.Suspended then
    fThread.Start
  else
    fMoveNext.SetEvent;

  fYield.WaitFor(INFINITE);
  Result := not fThread.Terminated;
end;

procedure TIteratorThread.Reset;
begin
  raise EInvalidOpException.Create('Reset');
end;

class procedure TIteratorThread.Yield(const value);
begin
  with fCurrentThread do
  begin
    SetCurrent(value);
    fYield.SetEvent;
    fMoveNext.WaitFor(INFINITE);
    if fThread.Terminated then
      Abort;
  end;
end;

{ TIteratorThread<T> }

function TIteratorThread<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TIteratorThread<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(fCurrent);
end;

procedure TIteratorThread<T>.SetCurrent(const value);
begin
  fCurrent := T(value);
end;

procedure TIteratorThread<T>.Yield(const value: TValue);
var
  lValue: T;
begin
  lValue := value.AsType<T>;
  inherited Yield(lValue);
end;

end.
