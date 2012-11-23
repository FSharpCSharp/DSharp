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

  TEventTest1 = procedure(const AInterface: IInterface; var AKey: Word;
    var AKeyChar: WideChar; AShift: TShiftState) of object;

  {$M+}
  TEventTest2 = reference to procedure;
  TEventTest3 = reference to procedure(const AValue: Int64);
  TEventTest4 = reference to procedure(const AValue: TNotifyEvent);

  TEventTest5 = reference to procedure(AValue: TRecord; i: Integer);
  {$M-}

  TEventTestCase = class(TTestCase)
  private
    FEventCalled: Boolean;

    FEvent1: Event<TEventTest1>;
    FEvent2, FEvent3: Event<TEventTest2>;

    FEvent4: Event<TEventTest3>;

    FEvent5: Event<TEventTest4>;

    FEvent6: Event<TEventTest5>;

    procedure TestEvent1(const AInterface: IInterface; var AKey: Word;
      var AKeyChar: WideChar; AShift: TShiftState);

    procedure TestEvent2;
    procedure TestEvent3;
    procedure TestEvent4(Sender: TObject);
  published
    procedure TestSignatureWithSet;

    procedure TestCascadeInvoke;

    procedure TestConstInt64;

    procedure TestEventParam;

    procedure TestRecordParam;
  end;

{ TEventTestCase }

procedure TEventTestCase.TestEvent1(const AInterface: IInterface;
  var AKey: Word; var AKeyChar: WideChar; AShift: TShiftState);
begin
  FEventCalled := True;
  CheckTrue(AKey = 0);
  CheckTrue(AKeyChar = 'K');
  CheckTrue(AShift = []);
end;

procedure TEventTestCase.TestEvent2;
begin
  FEvent3.Invoke;
end;

procedure TEventTestCase.TestEvent3;
begin
  FEventCalled := True;
end;

procedure TEventTestCase.TestEvent4(Sender: TObject);
begin
  FEventCalled := Sender = Self;
end;

procedure TEventTestCase.TestEventParam;
begin
  FEvent5.Add(
    procedure(const AValue: TNotifyEvent)
    begin
      AValue(Self);
    end);
  FEvent5.Invoke(TestEvent4);
  CheckTrue(FEventCalled);
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
  FEvent6.Add(
    procedure(AValue: TRecord; i: Integer)
    begin
      FEventCalled := (AValue.a = 1) and (AValue.b = 2) and (AValue.c = 3)
        and (AValue.d = 4) and (AValue.e = 5) and (i = 6);
      AValue.c := 7;
    end);
  FEvent6.Invoke(r, 6);
  CheckTrue(FEventCalled);
  CheckEquals(r.c, 3);
end;

procedure TEventTestCase.TestSignatureWithSet;
var
  Key: Word;
  KeyChar: WideChar;
begin
  FEvent1.Add(TestEvent1);
  Key := 0;
  KeyChar := 'K';
  FEvent1.Invoke(nil, Key, KeyChar, []);
  CheckTrue(FEventCalled);
end;

procedure TEventTestCase.TestCascadeInvoke;
begin
  FEvent2.Add(TestEvent2);
  FEvent3.Add(TestEvent3);
  FEvent3.Invoke();
  CheckTrue(FEventCalled);
end;

procedure TEventTestCase.TestConstInt64;
begin
  FEvent4.Add(
    procedure(const AValue: Int64)
    begin
      FEventCalled := AValue = 1;
    end);
  FEvent4.Invoke(1);
  CheckTrue(FEventCalled);
end;

initialization
  RegisterTest('DSharp.Core.Events', TEventTestCase.Suite);

end.
