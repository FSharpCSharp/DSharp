unit DSharp.Core.Times.Tests;

interface

implementation

uses
  DSharp.Core.Times,
  TestFramework;

type
  TTimesTestCase = class(TTestCase)
  private
    FTimes: Times;
  published
    procedure Verify_AtLeastOnce_One_True;
    procedure Verify_AtLeastOnce_Two_True;
    procedure Verify_AtLeastOnce_Zero_False;
    procedure Verify_AtLeastZero_One_True;
    procedure Verify_AtLeastZero_Zero_True;
    procedure Verify_AtMostOnce_One_True;
    procedure Verify_AtMostOnce_Two_True;
    procedure Verify_AtMostOnce_Zero_True;
    procedure Verify_AtMostZero_One_False;
    procedure Verify_AtMostZero_Zero_True;
    procedure Verify_BetweenOneAndTwo_One_True;
    procedure Verify_BetweenOneAndTwo_Three_False;
    procedure Verify_BetweenOneAndTwo_Two_True;
    procedure Verify_BetweenOneAndTwo_Zero_False;
    procedure Verify_ExactlyOnce_One_True;
    procedure Verify_ExactlyOnce_Two_False;
    procedure Verify_ExactlyOnce_Zero_False;
    procedure Verify_ExactlyZero_One_False;
    procedure Verify_ExactlyZero_Zero_True;
  end;

{ TTimesTestCase }

procedure TTimesTestCase.Verify_AtLeastOnce_One_True;
begin
  FTimes := Times.AtLeastOnce;
  CheckTrue(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_AtLeastOnce_Two_True;
begin
  FTimes := Times.AtLeastOnce;
  CheckTrue(FTimes.Verify(2));
end;

procedure TTimesTestCase.Verify_AtLeastOnce_Zero_False;
begin
  FTimes := Times.AtLeastOnce;
  CheckFalse(FTimes.Verify(0));
end;

procedure TTimesTestCase.Verify_AtLeastZero_One_True;
begin
  FTimes := Times.AtLeast(0);
  CheckTrue(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_AtLeastZero_Zero_True;
begin
  FTimes := Times.AtLeast(0);
  CheckTrue(FTimes.Verify(0));
end;

procedure TTimesTestCase.Verify_AtMostOnce_One_True;
begin
  FTimes := Times.AtMostOnce;
  CheckTrue(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_AtMostOnce_Two_True;
begin
  FTimes := Times.AtMostOnce;
  CheckFalse(FTimes.Verify(2));
end;

procedure TTimesTestCase.Verify_AtMostOnce_Zero_True;
begin
  FTimes := Times.AtMostOnce;
  CheckTrue(FTimes.Verify(0));
end;

procedure TTimesTestCase.Verify_AtMostZero_One_False;
begin
  FTimes := Times.AtMost(0);
  CheckFalse(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_AtMostZero_Zero_True;
begin
  FTimes := Times.AtMost(0);
  CheckTrue(FTimes.Verify(0));
end;

procedure TTimesTestCase.Verify_BetweenOneAndTwo_One_True;
begin
  FTimes := Times.Between(1, 2);
  CheckTrue(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_BetweenOneAndTwo_Three_False;
begin
  FTimes := Times.Between(1, 2);
  CheckFalse(FTimes.Verify(3));
end;

procedure TTimesTestCase.Verify_BetweenOneAndTwo_Two_True;
begin
  FTimes := Times.Between(1, 2);
  CheckTrue(FTimes.Verify(2));
end;

procedure TTimesTestCase.Verify_BetweenOneAndTwo_Zero_False;
begin
  FTimes := Times.Between(1, 2);
  CheckFalse(FTimes.Verify(0));
end;

procedure TTimesTestCase.Verify_ExactlyOnce_One_True;
begin
  FTimes := Times.Exactly(1);
  CheckTrue(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_ExactlyOnce_Two_False;
begin
  FTimes := Times.Exactly(1);
  CheckFalse(FTimes.Verify(2));
end;

procedure TTimesTestCase.Verify_ExactlyOnce_Zero_False;
begin
  FTimes := Times.Exactly(1);
  CheckFalse(FTimes.Verify(0));
end;

procedure TTimesTestCase.Verify_ExactlyZero_One_False;
begin
  FTimes := Times.Exactly(0);
  CheckFalse(FTimes.Verify(1));
end;

procedure TTimesTestCase.Verify_ExactlyZero_Zero_True;
begin
  FTimes := Times.Exactly(0);
  CheckTrue(FTimes.Verify(0));
end;

initialization
  RegisterTest('DSharp.Core.Times', TTimesTestCase.Suite);

end.
