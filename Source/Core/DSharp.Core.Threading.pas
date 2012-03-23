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
  IFuture = interface
    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel;
    procedure WaitFor;
  end;

  TAbstractFutureThread = class;

  TAbstractFuture = class(TInterfacedObject, IFuture)
  strict protected
    FCanceled: Boolean;
    FWorker: TAbstractFutureThread;
  public
    destructor Destroy; override;

    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel; virtual;
    procedure WaitFor; virtual;
  end;

  TAbstractFutureThread = class(TThread)
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

  TFuture = class(TAbstractFuture)
  public
    constructor Create(const AAction: TProc);
  end;

  TFutureThread = class(TAbstractFutureThread)
  strict private
    FAction: TProc;
  public
    constructor Create(const AAction: TProc);
    procedure Execute; override;
  end;

  IFuture<T> = interface(IFuture)
    function Value: T;
  end;

  TFuture<T> = class(TAbstractFuture, IFuture<T>)
  public
    constructor Create(const AAction: TFunc<T>);

    function Value: T;
  end;

  TFutureThread<T> = class(TAbstractFutureThread)
  strict private
    FAction: TFunc<T>;
    FResult: T;
  public
    constructor Create(const AAction: TFunc<T>);
    procedure Execute; override;
    property Result: T read FResult;
  end;

implementation

type
  TThreadHelper = class helper for TThread
  protected
    function DetachException: Exception;
  end;

{ TAbstractFuture }

destructor TAbstractFuture.Destroy;
begin
  FreeAndNil(FWorker);
  inherited;
end;

procedure TAbstractFuture.Cancel;
begin
  if FCanceled then
    raise Exception.Create('Action already canceled');

  if not FWorker.Finished then
  begin
    FWorker.Terminate();
    FCanceled := True;
  end;
end;

function TAbstractFuture.Canceled: Boolean;
begin
  Result := FCanceled;
end;

function TAbstractFuture.Finished: Boolean;
begin
  Result := FWorker.Finished;
end;

procedure TAbstractFuture.WaitFor;
begin
  FWorker.WaitFor();
end;

{ TAbstractFutureThread }

constructor TAbstractFutureThread.Create;
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

destructor TAbstractFutureThread.Destroy;
begin
  if not Finished and not Terminated then
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

procedure TAbstractFutureThread.Continue;
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

procedure TAbstractFutureThread.DoTerminate;
begin
  inherited;
  FFinished.SetEvent;
end;

procedure TAbstractFutureThread.RaiseException;
var
  E: Exception;
begin
  E := DetachException;
  if Assigned(E) and not (E is EAbort) then
  begin
    raise E;
  end;
end;

procedure TAbstractFutureThread.Yield;
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

{ TFuture }

constructor TFuture.Create(const AAction: TProc);
begin
  inherited Create();
  FWorker := TFutureThread.Create(AAction);
  FWorker.Start();
end;

{ TFutureThread }

constructor TFutureThread.Create(const AAction: TProc);
begin
  inherited Create();
  FAction := AAction;
end;

procedure TFutureThread.Execute;
begin
  inherited;
  FAction();
end;

{ TFutureThread<T> }

constructor TFutureThread<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create();
  FAction := AAction;
end;

procedure TFutureThread<T>.Execute;
begin
  inherited;
  FResult := FAction();
end;

{ TFuture<T> }

constructor TFuture<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create;
  FWorker := TFutureThread<T>.Create(AAction);
  FWorker.Start();
end;

function TFuture<T>.Value: T;
begin
  if FCanceled then
    raise Exception.Create('Action was canceled');

  if not Finished then
  begin
    WaitFor();
  end;
  Result := TFutureThread<T>(FWorker).Result;
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

end.
