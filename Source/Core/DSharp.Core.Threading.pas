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

unit DSharp.Core.Threading;

{$IFNDEF MSWINDOWS}
{$MESSAGE WARN 'Unit only supports Windows'}
{$ENDIF}

interface

uses
  Classes,
  SyncObjs,
  SysUtils;

type
  ITask = interface
    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel;
    procedure Start;
    procedure Wait;
  end;

  ITask<T> = interface(ITask)
    function Value: T;
  end;

  TAbstractTaskThread = class(TThread)
  strict private
    FFinishedOrYielded: THandleObjectArray;
    FTerminatedOrResumed: THandleObjectArray;
    FFinished: TEvent;
    FResumed: TEvent;
    FTerminated: TEvent;
    FYielded: TEvent;
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
  strict protected
    FCanceled: Boolean;
    FWorker: TAbstractTaskThread;
  public
    destructor Destroy; override;

    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel; virtual;
    procedure Start; virtual;
    procedure Wait; virtual;
  end;

  TTaskThread = class(TAbstractTaskThread)
  strict private
    FAction: TProc;
  public
    constructor Create(const AAction: TProc);
    procedure Execute; override;
  end;

  TTask = class(TAbstractTask)
  public
    constructor Create(const AAction: TProc);
  end;

  TTaskThread<T> = class(TAbstractTaskThread)
  strict private
    FAction: TFunc<T>;
    FResult: T;
  public
    constructor Create(const AAction: TFunc<T>);
    procedure Execute; override;
    property Result: T read FResult;
  end;

  TTask<T> = class(TAbstractTask, ITask<T>)
  public
    constructor Create(const AAction: TFunc<T>);

    function Value: T;
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
  begin
    Result := nil;
  end;
end;

{ TAbstractTaskThread }

constructor TAbstractTaskThread.Create;
begin
  inherited Create(True);
  FFinished := TEvent.Create(nil, False, False, '');
  FResumed := TEvent.Create(nil, False, False, '');
  FTerminated := TEvent.Create(nil, False, False, '');
  FYielded := TEvent.Create(nil, False, False, '');
  SetLength(FFinishedOrYielded, 2);
  SetLength(FTerminatedOrResumed, 2);
  FFinishedOrYielded[0] := FFinished;
  FFinishedOrYielded[1] := FYielded;
  FTerminatedOrResumed[0] := FTerminated;
  FTerminatedOrResumed[1] := FResumed;
end;

destructor TAbstractTaskThread.Destroy;
begin
  if not Suspended and not Finished and not Terminated then
  begin
    Terminate;
    FTerminated.SetEvent;
    WaitFor;
  end;

  FFinished.Free;
  FResumed.Free;
  FTerminated.Free;
  FYielded.Free;

  inherited;
end;

procedure TAbstractTaskThread.Continue;
{$IFDEF MSWINDOWS}
var
  LSignaledObject: THandleObject;
{$ENDIF}
begin
  if not Finished and not Terminated then
  begin
    if Suspended then
    begin
      Start;
    end
    else
    begin
      FResumed.SetEvent;
    end;
{$IFDEF MSWINDOWS}
    THandleObject.WaitForMultiple(FFinishedOrYielded, INFINITE, False, LSignaledObject);
{$ENDIF}
  end;
end;

procedure TAbstractTaskThread.DoTerminate;
begin
  inherited;
  FFinished.SetEvent;
end;

procedure TAbstractTaskThread.RaiseException;
var
  E: Exception;
begin
  E := DetachException;
  if Assigned(E) and not (E is EAbort) then
  begin
    raise E;
  end;
end;

procedure TAbstractTaskThread.Yield;
{$IFDEF MSWINDOWS}
var
  LSignaledObject: THandleObject;
{$ENDIF}
begin
  FYielded.SetEvent;
{$IFDEF MSWINDOWS}
  THandleObject.WaitForMultiple(FTerminatedOrResumed, INFINITE, False, LSignaledObject);
{$ENDIF}
  if Terminated then
    Abort;
end;

{ TAbstractTask }

destructor TAbstractTask.Destroy;
begin
  FreeAndNil(FWorker);
  inherited;
end;

procedure TAbstractTask.Cancel;
begin
  if FCanceled then
    raise Exception.Create('Action already canceled');

  if not FWorker.Finished then
  begin
    FWorker.Terminate();
    FCanceled := True;
  end;
end;

function TAbstractTask.Canceled: Boolean;
begin
  Result := FCanceled;
end;

function TAbstractTask.Finished: Boolean;
begin
  Result := FWorker.Finished;
end;

procedure TAbstractTask.Start;
begin
  FWorker.Start();
end;

procedure TAbstractTask.Wait;
begin
  FWorker.WaitFor();
end;

{ TTaskThread }

constructor TTaskThread.Create(const AAction: TProc);
begin
  inherited Create();
  FAction := AAction;
end;

procedure TTaskThread.Execute;
begin
  inherited;
  FAction();
end;

{ TTask }

constructor TTask.Create(const AAction: TProc);
begin
  inherited Create();
  FWorker := TTaskThread.Create(AAction);
end;

{ TTaskThread<T> }

constructor TTaskThread<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create();
  FAction := AAction;
end;

procedure TTaskThread<T>.Execute;
begin
  inherited;
  FResult := FAction();
end;

{ TTask<T> }

constructor TTask<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create;
  FWorker := TTaskThread<T>.Create(AAction);
end;

function TTask<T>.Value: T;
begin
  if FCanceled then
    raise Exception.Create('Action was canceled');

  if not Finished then
  begin
    Wait();
  end;
  Result := TTaskThread<T>(FWorker).Result;
end;

end.
