(*
  Copyright (c) 2011-2014, Stefan Glienke
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
  Classes,
  DSharp.Core.Threading,
  SysUtils;

type
  Async = record
  private
    fTask: ITask;
  public
    class operator Implicit(const value: TProc): Async;

    procedure Await;
    procedure Cancel;
  end;

  Async<T> = record
  private
    fTask: ITask<T>;
  public
    class operator Implicit(const value: TFunc<T>): Async<T>;
    class operator Implicit(const value: Async<T>): T;

    function Await: T;
    procedure Cancel;
  end;

  TAsync = class(TTask)
  public
    procedure Wait; override;
  end;

  TAsync<T> = class(TTask<T>)
  public
    procedure Wait; override;
  end;

function AsyncCanceled: Boolean;
procedure Delay(milliseconds: Integer);
procedure Synchronize(const proc: TProc);
procedure WaitFor(const thread: TThread);

implementation

uses
  Forms,
  Windows;

{$BOOLEVAL OFF}

function AsyncCanceled: Boolean;
begin
  Result := (TThread.CurrentThread.ThreadID <> MainThreadID) and TThread.CheckTerminated;
end;

procedure Delay(milliseconds: Integer);
var
  ticks: Cardinal;
  event: THandle;
begin
  event := CreateEvent(nil, False, False, nil);
  try
    ticks := GetTickCount + Cardinal(milliseconds);
    while (milliseconds > 0) and (MsgWaitForMultipleObjects(1, event, False,
      milliseconds, QS_ALLINPUT) <> WAIT_TIMEOUT) do
    begin
      Application.ProcessMessages;
      milliseconds := ticks - GetTickCount;
    end;
  finally
    CloseHandle(event);
  end;
end;

procedure Synchronize(const proc: TProc);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(TThread.CurrentThread, TThreadProcedure(proc))
  else
    proc;
end;

procedure WaitFor(const thread: TThread);
var
  handles: array[0..1] of THandle;
begin
  handles[0] := thread.Handle;
  if GetCurrentThreadId = MainThreadID then
  begin
    handles[1] := SyncEvent;
    repeat
      case MsgWaitForMultipleObjects(2, handles, False, INFINITE, QS_ALLINPUT) of
        WAIT_OBJECT_0 + 1: CheckSynchronize;
        WAIT_OBJECT_0 + 2: Application.ProcessMessages;
      end;
    until thread.Finished;
  end
  else
    WaitForSingleObject(handles[0], INFINITE);
end;

{ Async }

procedure Async.Await;
begin
  if Assigned(fTask) then
    fTask.Wait;
end;

procedure Async.Cancel;
begin
  if Assigned(fTask) then
    fTask.Cancel;
end;

class operator Async.Implicit(const value: TProc): Async;
begin
  Result.fTask := TAsync.Create(value);
  Result.fTask.Start;
end;

{ Async<T> }

function Async<T>.Await: T;
begin
  if Assigned(fTask) then
    Result := fTask.Result
  else
    Result := Default(T);
end;

procedure Async<T>.Cancel;
begin
  if Assigned(fTask) then
    fTask.Cancel;
end;

class operator Async<T>.Implicit(const value: TFunc<T>): Async<T>;
begin
  Result.fTask := TAsync<T>.Create(value);
  Result.fTask.Start;
end;

class operator Async<T>.Implicit(const value: Async<T>): T;
begin
  Result := value.Await;
end;

{ TAsync }

procedure TAsync.Wait;
begin
  WaitFor(FWorker);
  FWorker.RaiseException;
end;

{ TAsync<T> }

procedure TAsync<T>.Wait;
begin
  WaitFor(FWorker);
  FWorker.RaiseException;
end;

end.
