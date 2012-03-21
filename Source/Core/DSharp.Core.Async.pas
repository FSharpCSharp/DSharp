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

unit DSharp.Core.Async;

interface

uses
  DSharp.Core.Threading,
  SysUtils;

type
  Async<T> = record
  private
    FFuture: IFuture<T>;
    function GetAwait: T;
  public
    class operator Implicit(const Value: TFunc<T>): Async<T>;
    class operator Implicit(const Value: Async<T>): T;

    procedure Cancel;
    property Await: T read GetAwait;
  end;

  TAsync<T> = class(TFuture<T>, IFuture<T>)
    function Value: T;
  end;

function AsyncCanceled: Boolean;
procedure Delay(Milliseconds: Integer);
procedure Synchronize(AProc: TProc);

implementation

uses
  Classes,
  Forms,
  Windows;

{$BOOLEVAL OFF}

function AsyncCanceled: Boolean;
begin
  Result := (GetCurrentThreadId() <> MainThreadID) and TThread.CheckTerminated;
end;

procedure Delay(Milliseconds: Integer);
var
  Tick: Cardinal;
  Event: THandle;
begin
  Event := CreateEvent(nil, False, False, nil);
  try
    Tick := GetTickCount() + Cardinal(Milliseconds);
    while (Milliseconds > 0) and (MsgWaitForMultipleObjects(1, Event, False,
      Milliseconds, QS_ALLINPUT) <> WAIT_TIMEOUT) do
    begin
      Application.ProcessMessages();
      Milliseconds := Tick - GetTickCount();
    end;
  finally
    CloseHandle(Event);
  end;
end;

procedure Synchronize(AProc: TProc);
begin
  if GetCurrentThreadId() <> MainThreadID then
  begin
    TThread.Synchronize(TThread.CurrentThread, TThreadProcedure(AProc));
  end
  else
  begin
    AProc();
  end;
end;

{ Async<T> }

procedure Async<T>.Cancel;
begin
  FFuture.Cancel();
end;

function Async<T>.GetAwait: T;
begin
  if Assigned(FFuture) then
  begin
    Result := FFuture.Value;
  end
  else
  begin
    Result := Default(T);
  end;
end;

class operator Async<T>.Implicit(const Value: TFunc<T>): Async<T>;
begin
  Result.FFuture := TAsync<T>.Create(Value);
end;

class operator Async<T>.Implicit(const Value: Async<T>): T;
begin
  Result := Value.FFuture.Value;
end;

{ TAsync<T> }

function TAsync<T>.Value: T;
var
  LHandles: array[0..1] of THandle;
begin
  LHandles[0] := FWorker.Handle;
  if GetCurrentThreadId = MainThreadID then
  begin
    LHandles[1] := SyncEvent;
    repeat
      case MsgWaitForMultipleObjects(2, LHandles, False, INFINITE, QS_ALLINPUT) of
        WAIT_OBJECT_0 + 1: CheckSynchronize();
        WAIT_OBJECT_0 + 2: Application.ProcessMessages();
      end;
    until Finished;
  end
  else
  begin
    WaitForSingleObject(LHandles[0], INFINITE);
  end;

  Result := TFutureThread<T>(FWorker).Result;
end;

end.
