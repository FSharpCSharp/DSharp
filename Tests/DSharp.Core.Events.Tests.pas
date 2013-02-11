unit DSharp.Core.Events.Tests;

interface

implementation

uses
  Classes,
  DSharp.Core.Events,
  Generics.Collections,
  TestFramework;

type
  TRecord = record
    a, b, c, d, e: Integer;
  end;

  TKeyEvent = procedure(const AInterface: IInterface; var AKey: Word;
    var AKeyChar: WideChar; AShift: TShiftState) of object;

  {$M+}
  TProc = reference to procedure;
  TProcInt64 = reference to procedure(const AValue: Int64);
  TProcNotifyEvent = reference to procedure(const AValue: TNotifyEvent);
  TProcRecordInt = reference to procedure(AValue: TRecord; i: Integer);
  TNotifyProc = reference to procedure(Sender: TObject);
  {$M-}

  TNotifyProcEvent = class(TEventBase<TNotifyProc>)
    procedure InternalInvoke(Sender: TObject);
    procedure InitInvoke; override;
  end;

  TEventTestCase = class(TTestCase)
  private
    FEventCalled: Boolean;
    FKeyEvent: Event<TKeyEvent>;
    FProc1, FProc2: Event<TProc>;
    FProcInt64: Event<TProcInt64>;
    FProcNotifyEvent: Event<TProcNotifyEvent>;
    FProcRecordInt: Event<TProcRecordInt>;
    FNotifyEvent: TEvent<TNotifyEvent>;

    procedure TestKeyEvent(const AInterface: IInterface; var AKey: Word;
      var AKeyChar: WideChar; AShift: TShiftState);
    procedure TestProc1;
    procedure TestProc2;
    procedure TestNotifyEvent(Sender: TObject);
  published
    procedure TestSignatureWithSet;
    procedure TestCascadeInvoke;
    procedure TestConstInt64;
    procedure TestEventParam;
    procedure TestRecordParam;
    procedure TestDelegateMethodCompatibility;
    procedure TestMemoryManagement;
    procedure TestReferenceCounting;
  end;

{ TNotifyProcEvent }

procedure TNotifyProcEvent.InternalInvoke(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Handler[i].Invoke(Sender);
end;

procedure TNotifyProcEvent.InitInvoke;
begin
  SetInvoke(@TNotifyProcEvent.InternalInvoke);
end;

{ TEventTestCase }

procedure TEventTestCase.TestKeyEvent(const AInterface: IInterface;
  var AKey: Word; var AKeyChar: WideChar; AShift: TShiftState);
begin
  FEventCalled := True;
  CheckTrue(AKey = 0);
  CheckTrue(AKeyChar = 'K');
  CheckTrue(AShift = []);
end;

procedure TEventTestCase.TestProc1;
begin
  FProc1.Invoke;
end;

procedure TEventTestCase.TestProc2;
begin
  FEventCalled := True;
end;

procedure TEventTestCase.TestNotifyEvent(Sender: TObject);
begin
  FEventCalled := Sender = Self;
end;

procedure TEventTestCase.TestEventParam;
begin
  FProcNotifyEvent.Add(
    procedure(const AValue: TNotifyEvent)
    begin
      AValue(Self);
    end);
  FProcNotifyEvent.Invoke(TestNotifyEvent);
  CheckTrue(FEventCalled);
end;

type
  TTest = class
    procedure Handler(Sender: TObject);
  end;

procedure TTest.Handler(Sender: TObject);
begin
end;

procedure TEventTestCase.TestMemoryManagement;
var
  e: IEvent<TNotifyEvent>;
  t: TTest;
  p: PByte;
begin
  t := TTest.Create;
  e := TEvent<TNotifyEvent>.Create;
  e.Add(t.Handler);

  p := PByte(t);
  // free instance before the event its handler is attached to gets freed
  t.Free;
  // simulate memory reuse (the ugly way)
  p^ := 255;

  // at the end of the method the event gets freed and calls Notify for each attached handler
  // this may cause errors when that method does not check for the TMethod.Data part correctly
  // it needs to do so for delegate types
end;

procedure TEventTestCase.TestRecordParam;
var
  r: TRecord;
begin
  r.a := 1;
  r.b := 2;
  r.c := 3;
  r.d := 4;
  r.e := 5;
  FProcRecordInt.Add(
    procedure(AValue: TRecord; i: Integer)
    begin
      FEventCalled := (AValue.a = 1) and (AValue.b = 2) and (AValue.c = 3)
        and (AValue.d = 4) and (AValue.e = 5) and (i = 6);
      AValue.c := 7;
    end);
  FProcRecordInt.Invoke(r, 6);
  CheckTrue(FEventCalled);
  CheckEquals(r.c, 3);
end;

procedure TEventTestCase.TestReferenceCounting;
var
  e: IEvent<TNotifyProc>;
begin
  e := TNotifyProcEvent.Create;
  e.Add(procedure(Sender: TObject) begin FEventCalled := Sender = Self end);
  e.Add(procedure(Sender: TObject) begin FEventCalled := Sender = Self end);
  e.Invoke(Self);
end;

procedure TEventTestCase.TestSignatureWithSet;
var
  Key: Word;
  KeyChar: WideChar;
begin
  FKeyEvent.Add(TestKeyEvent);
  Key := 0;
  KeyChar := 'K';
  FKeyEvent.Invoke(nil, Key, KeyChar, []);
  CheckTrue(FEventCalled);
end;

procedure TEventTestCase.TestCascadeInvoke;
begin
  FProc1.Add(TestProc1);
  FProc2.Add(TestProc2);
  FProc2.Invoke();
  CheckTrue(FEventCalled);
end;

procedure TEventTestCase.TestConstInt64;
begin
  FProcInt64.Add(
    procedure(const AValue: Int64)
    begin
      FEventCalled := AValue = 1;
    end);
  FProcInt64.Invoke(1);
  CheckTrue(FEventCalled);
end;

procedure TEventTestCase.TestDelegateMethodCompatibility;
begin
  FNotifyEvent := TEvent<TNotifyEvent>.Create;
  FNotifyEvent.Add<TNotifyProc>(
    procedure(Sender: TObject)
    begin
      FEventCalled := Sender = Self;
    end);
  FNotifyEvent.Invoke(Self);
  CheckTrue(FEventCalled);
  FNotifyEvent.Free;
end;

initialization
  RegisterTest('DSharp.Core.Events', TEventTestCase.Suite);

end.
