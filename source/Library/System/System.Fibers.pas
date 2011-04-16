unit System.Fibers;

interface

uses
  Classes,
  SysUtils;

type
  TFiber = class(TInterfacedObject)
  strict private
    FBaseHandle: Pointer;
    FException: Exception;
    FFinished: Boolean;
    FFreeOnTerminate: Boolean;
    FHandle: Pointer;
    FThread: TThread;
  private
    procedure Run;
  strict protected
    procedure Execute; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    class function CurrentFiber: TFiber;
    class procedure Initialize;

    procedure HandleException;

    procedure Resume;
    procedure Yield;

    procedure Synchronize(AThreadProc: TThreadProcedure);
    procedure SwitchToMainThread;
    procedure SwitchToWorkerThread;

    property FreeOnTerminate: Boolean
      read FFreeOnTerminate write FFreeOnTerminate;
    property Finished: Boolean read FFinished;
  end;

  TActionFiber = class(TFiber)
  strict private
    FAction: TProc;
    FThreaded: Boolean;
  strict protected
    procedure Execute; override;
    property Action: TProc read FAction write FAction;
    property Threaded: Boolean read FThreaded write FThreaded;
  public
    constructor Create(const AAction: TProc; const AThreaded: Boolean = True);
  end;

implementation

uses
  Windows;

{$REGION 'Fiber Functions'}
function ConvertFiberToThread: Pointer; stdcall; external kernel32;
function ConvertThreadToFiber(lpParameter: Pointer): Pointer; stdcall; external kernel32;
function CreateFiber(dwStackSize: Cardinal; lpStartAddress: Pointer;
  lpParameter: Pointer): Pointer; stdcall; external kernel32;
function DeleteFiber(lpFiber: Pointer): Boolean; stdcall; external kernel32;
function SwitchToFiber(lpFiber: Pointer): Boolean; stdcall; external kernel32;

function GetCurrentFiber: Pointer;
asm
  MOV EAX, FS:[$10]
end;

function GetFiberData: Pointer;
asm
  MOV EAX, FS:[$10]
  MOV EAX, [EAX]
end;

procedure GlobalStartFiber;
var
  LFiber: TFiber;
begin
  LFiber := TFiber.CurrentFiber;
  LFiber.Run;
end;
{$ENDREGION}

type
  TFiberThread = class(TThread)
  strict private
    FFiber: TFiber;
  strict protected
    procedure Execute; override;
  public
    constructor Create(const AFiber: TFiber);
  end;

threadvar
  FiberCount: Integer;
  ThreadFiber: Pointer;

var
  MainFiber: Pointer;

{ TFiber }

constructor TFiber.Create;
begin
  Initialize;
  FHandle := CreateFiber(0, @GlobalStartFiber, Self);
  FBaseHandle := GetCurrentFiber;
end;

destructor TFiber.Destroy;
begin
  Dec(FiberCount);
  if not FFreeOnTerminate then
  begin
    DeleteFiber(FHandle);
    if Assigned(ThreadFiber) and (FiberCount = 0) then
    begin
      ConvertFiberToThread;
      ThreadFiber := nil;
    end;
  end;
  inherited;
end;

class function TFiber.CurrentFiber: TFiber;
begin
  if FiberCount > 0 then
  begin
    Result := TFiber(GetFiberData);
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TFiber.HandleException;
var
  E: Exception;
begin
  if FException <> nil then
  begin
    E := FException;
    FException := nil;
    raise E;
  end;
end;

class procedure TFiber.Initialize;
begin
  if not Assigned(ThreadFiber) then
  begin
    ThreadFiber := ConvertThreadToFiber(nil);
    if not Assigned(MainFiber) then
    begin
      MainFiber := ThreadFiber;
    end;
  end;
  Inc(FiberCount);
end;

procedure TFiber.Resume;
begin
  if not FFinished then
  begin
    SwitchToFiber(FHandle);
  end;
end;

procedure TFiber.Run;
begin
  try
    Execute;
  except
    on EAbort do;
    on Exception do
      FException := AcquireExceptionObject;
  end;
  FFinished := True;
  if FFreeOnTerminate then
  begin
    Free;
  end;
  SwitchToMainThread;
  Yield;
end;

procedure TFiber.SwitchToMainThread;
begin
  if Assigned(FThread) and (GetCurrentThreadId = FThread.ThreadID) then
  begin
    FThread.Terminate;
    FThread := nil;
    SwitchToFiber(ThreadFiber);
  end;
end;

procedure TFiber.SwitchToWorkerThread;
begin
  if GetCurrentThreadID = MainThreadID then
  begin
    FThread := TFiberThread.Create(Self);
    FThread.FreeOnTerminate := True;
    Yield;
  end;
end;

procedure TFiber.Synchronize(AThreadProc: TThreadProcedure);
begin
  TThread.Synchronize(FThread, AThreadProc);
end;

procedure TFiber.Yield;
begin
  SwitchToFiber(FBaseHandle);
end;

{ TFiberThread }

constructor TFiberThread.Create(const AFiber: TFiber);
begin
  inherited Create(False);
  FFiber := AFiber;
end;

procedure TFiberThread.Execute;
begin
  TFiber.Initialize;
  while not Terminated do
  begin
    FFiber.Resume;
  end;
  TThread.Synchronize(nil, FFiber.Resume);
end;

{ TActionFiber }

procedure TActionFiber.Execute;
begin
  if FThreaded then
  begin
    SwitchToWorkerThread;
  end;
  FAction();
  if FThreaded then
  begin
    SwitchToMainThread;
  end;
end;

constructor TActionFiber.Create(const AAction: TProc; const AThreaded: Boolean);
begin
  inherited Create();
  Action := AAction;
  Threaded := AThreaded;
  FreeOnTerminate := True;
end;

end.
