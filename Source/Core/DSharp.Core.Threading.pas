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

unit DSharp.Core.Threading;

interface

uses
  Classes,
  SysUtils;

type
  ITask = interface
    function GetIsCanceled: Boolean;
    function GetIsCompleted: Boolean;

    procedure Cancel;
    procedure Start;
    procedure Wait;

    property IsCanceled: Boolean read GetIsCanceled;
    property IsCompleted: Boolean read GetIsCompleted;
  end;

  ITask<T> = interface(ITask)
    function GetResult: T;

    property Result: T read GetResult;
  end;

  TAbstractTaskThread = class(TThread)
  private
    fYielding: Boolean;
  protected
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Continue;
    procedure RaiseException;
    procedure Yield;
  end;

  TAbstractTask = class(TInterfacedObject, ITask)
  private
    function GetIsCanceled: Boolean;
    function GetIsCompleted: Boolean;
  protected
    fCanceled: Boolean;
    fWorker: TAbstractTaskThread;
  public
    destructor Destroy; override;

    procedure Cancel; virtual;
    procedure Start; virtual;
    procedure Wait; virtual;

    property IsCanceled: Boolean read GetIsCanceled;
    property IsCompleted: Boolean read GetIsCompleted;
  end;

  TTaskThread = class(TAbstractTaskThread)
  private
    fAction: TProc;
  public
    constructor Create(const action: TProc);

    procedure Execute; override;
  end;

  TTask = class(TAbstractTask)
  public
    constructor Create(const action: TProc);
  end;

  TTaskThread<T> = class(TAbstractTaskThread)
  private
    fAction: TFunc<T>;
    fResult: T;
  public
    constructor Create(const action: TFunc<T>);

    procedure Execute; override;

    property Result: T read fResult;
  end;

  TTask<T> = class(TAbstractTask, ITask<T>)
  private
    function GetResult: T;
  public
    constructor Create(const action: TFunc<T>);

    property Result: T read GetResult;
  end;

implementation

type
  TThreadHelper = class helper for TThread
  protected
    function DetachException: Exception;
  end;

{ TThreadHelper }

function TThreadHelper.DetachException: Exception;
begin
  if (FatalException is Exception) and not (FatalException is EAbort) then
  begin
    Result := Exception(FatalException);
    Self.FFatalException := nil;
  end
  else
    Result := nil;
end;

{ TAbstractTaskThread }

constructor TAbstractTaskThread.Create;
begin
  inherited Create(True);
end;

destructor TAbstractTaskThread.Destroy;
begin
  if not Suspended and not Finished and not Terminated then
  begin
    Terminate;
    TMonitor.PulseAll(Self);
  end;
  inherited;
end;

procedure TAbstractTaskThread.Continue;
begin
  TMonitor.Enter(Self);
  try
    if Suspended then
      Start;
    while not fYielding do
      TMonitor.Wait(Self, INFINITE);
    fYielding := False;
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TAbstractTaskThread.DoTerminate;
begin
  inherited;
  Yield;
end;

procedure TAbstractTaskThread.RaiseException;
var
  E: Exception;
begin
  E := DetachException;
  if Assigned(E) and not (E is EAbort) then
    raise E;
end;

procedure TAbstractTaskThread.Yield;
begin
  TMonitor.Enter(Self);
  try
    while fYielding and not Terminated do
      TMonitor.Wait(Self, INFINITE);
    fYielding := True;
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

{ TAbstractTask }

destructor TAbstractTask.Destroy;
begin
  FreeAndNil(fWorker);
  inherited;
end;

procedure TAbstractTask.Cancel;
begin
  if fCanceled then
    raise Exception.Create('Action already canceled');

  if not fWorker.Finished then
  begin
    fWorker.Terminate;
    fCanceled := True;
  end;
end;

function TAbstractTask.GetIsCanceled: Boolean;
begin
  Result := fCanceled;
end;

function TAbstractTask.GetIsCompleted: Boolean;
begin
  Result := fWorker.Finished;
end;

procedure TAbstractTask.Start;
begin
  fWorker.Start;
end;

procedure TAbstractTask.Wait;
begin
  fWorker.WaitFor;
end;

{ TTaskThread }

constructor TTaskThread.Create(const action: TProc);
begin
  inherited Create;
  fAction := action;
end;

procedure TTaskThread.Execute;
begin
  inherited;
  fAction;
end;

{ TTask }

constructor TTask.Create(const action: TProc);
begin
  inherited Create;
  fWorker := TTaskThread.Create(action);
end;

{ TTaskThread<T> }

constructor TTaskThread<T>.Create(const action: TFunc<T>);
begin
  inherited Create;
  fAction := action;
end;

procedure TTaskThread<T>.Execute;
begin
  inherited;
  fResult := fAction;
end;

{ TTask<T> }

constructor TTask<T>.Create(const action: TFunc<T>);
begin
  inherited Create;
  fWorker := TTaskThread<T>.Create(action);
end;

function TTask<T>.GetResult: T;
begin
  if fCanceled then
    raise Exception.Create('Action was canceled');

  if not IsCompleted then
    Wait;
  Result := TTaskThread<T>(fWorker).Result;
end;

end.
