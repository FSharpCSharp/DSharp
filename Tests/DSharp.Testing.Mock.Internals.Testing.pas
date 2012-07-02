unit DSharp.Testing.Mock.Internals.Testing;

interface

implementation

uses
  DSharp.Testing.Mock.Internals,
  TestFramework;

type
  TCountMatcherEqualTestCase = class(TTestCase)
  private
    FCountMatcher: TCountMatcher;
  published
    procedure AtLeastOnce_EqualOne_True;
    procedure AtLeastOnce_EqualTwo_True;
    procedure AtLeastOnce_EqualZero_False;
    procedure AtLeastZero_EqualOne_True;
    procedure AtLeastZero_EqualZero_True;
    procedure AtMostOnce_EqualOne_True;
    procedure AtMostOnce_EqualTwo_True;
    procedure AtMostOnce_EqualZero_True;
    procedure AtMostZero_EqualOne_False;
    procedure AtMostZero_EqualZero_True;
    procedure BetweenOneAndTwo_EqualOne_True;
    procedure BetweenOneAndTwo_EqualThree_False;
    procedure BetweenOneAndTwo_EqualTwo_True;
    procedure BetweenOneAndTwo_EqualZero_False;
    procedure ExactlyOnce_EqualOne_True;
    procedure ExactlyOnce_EqualTwo_False;
    procedure ExactlyOnce_EqualZero_False;
    procedure ExactlyZero_EqualOne_False;
    procedure ExactlyZero_EqualZero_True;
  end;

  TCountMatcherGreaterThanOrEqualTestCase = class(TTestCase)
  private
    FCountMatcher: TCountMatcher;
  published
    procedure AtLeastOnce_GreaterThanOrEqualOne_True;
    procedure AtLeastOnce_GreaterThanOrEqualTwo_True;
    procedure AtLeastOnce_GreaterThanOrEqualZero_True;
    procedure AtLeastZero_GreaterThanOrEqualOne_True;
    procedure AtLeastZero_GreaterThanOrEqualZero_True;
    procedure AtMostOnce_GreaterThanOrEqualOne_True;
    procedure AtMostOnce_GreaterThanOrEqualTwo_False;
    procedure AtMostOnce_GreaterThanOrEqualZero_True;
    procedure AtMostZero_GreaterThanOrEqualOne_False;
    procedure AtMostZero_GreaterThanOrEqualZero_True;
    procedure BetweenOneAndTwo_GreaterThanOrEqualOne_True;
    procedure BetweenOneAndTwo_GreaterThanOrEqualThree_False;
    procedure BetweenOneAndTwo_GreaterThanOrEqualTwo_True;
    procedure BetweenOneAndTwo_GreaterThanOrEqualZero_True;
    procedure ExactlyOnce_GreaterThanOrEqualOne_True;
    procedure ExactlyOnce_GreaterThanOrEqualTwo_False;
    procedure ExactlyOnce_GreaterThanOrEqualZero_True;
    procedure ExactlyZero_GreaterThanOrEqualOne_False;
    procedure ExactlyZero_GreaterThanOrEqualZero_True;
  end;

{ TCountMatcherEqualTestCase }

procedure TCountMatcherEqualTestCase.AtLeastOnce_EqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(1, Undefined);
  CheckTrue(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.AtLeastOnce_EqualTwo_True;
begin
  FCountMatcher := TCountMatcher.Create(1, Undefined);
  CheckTrue(FCountMatcher = 2);
end;

procedure TCountMatcherEqualTestCase.AtLeastOnce_EqualZero_False;
begin
  FCountMatcher := TCountMatcher.Create(1, Undefined);
  CheckFalse(FCountMatcher = 0);
end;

procedure TCountMatcherEqualTestCase.AtLeastZero_EqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(0, Undefined);
  CheckTrue(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.AtLeastZero_EqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(0, Undefined);
  CheckTrue(FCountMatcher = 0);
end;

procedure TCountMatcherEqualTestCase.AtMostOnce_EqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 1);
  CheckTrue(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.AtMostOnce_EqualTwo_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 1);
  CheckFalse(FCountMatcher = 2);
end;

procedure TCountMatcherEqualTestCase.AtMostOnce_EqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 1);
  CheckTrue(FCountMatcher = 0);
end;

procedure TCountMatcherEqualTestCase.AtMostZero_EqualOne_False;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 0);
  CheckFalse(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.AtMostZero_EqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 0);
  CheckTrue(FCountMatcher = 0);
end;

procedure TCountMatcherEqualTestCase.BetweenOneAndTwo_EqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckTrue(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.BetweenOneAndTwo_EqualThree_False;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckFalse(FCountMatcher = 3);
end;

procedure TCountMatcherEqualTestCase.BetweenOneAndTwo_EqualTwo_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckTrue(FCountMatcher = 2);
end;

procedure TCountMatcherEqualTestCase.BetweenOneAndTwo_EqualZero_False;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckFalse(FCountMatcher = 0);
end;

procedure TCountMatcherEqualTestCase.ExactlyOnce_EqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 1);
  CheckTrue(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.ExactlyOnce_EqualTwo_False;
begin
  FCountMatcher := TCountMatcher.Create(1, 1);
  CheckFalse(FCountMatcher = 2);
end;

procedure TCountMatcherEqualTestCase.ExactlyOnce_EqualZero_False;
begin
  FCountMatcher := TCountMatcher.Create(1, 1);
  CheckFalse(FCountMatcher = 0);
end;

procedure TCountMatcherEqualTestCase.ExactlyZero_EqualOne_False;
begin
  FCountMatcher := TCountMatcher.Create(0, 0);
  CheckFalse(FCountMatcher = 1);
end;

procedure TCountMatcherEqualTestCase.ExactlyZero_EqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(0, 0);
  CheckTrue(FCountMatcher = 0);
end;

{ TCountMatcherGreaterThanOrEqualTestCase }

procedure TCountMatcherGreaterThanOrEqualTestCase.AtLeastOnce_GreaterThanOrEqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(1, Undefined);
  CheckTrue(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtLeastOnce_GreaterThanOrEqualTwo_True;
begin
  FCountMatcher := TCountMatcher.Create(1, Undefined);
  CheckTrue(FCountMatcher >= 2);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtLeastOnce_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(1, Undefined);
  CheckTrue(FCountMatcher >= 0);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtLeastZero_GreaterThanOrEqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(0, Undefined);
  CheckTrue(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtLeastZero_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(0, Undefined);
  CheckTrue(FCountMatcher >= 0);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtMostOnce_GreaterThanOrEqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 1);
  CheckTrue(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtMostOnce_GreaterThanOrEqualTwo_False;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 1);
  CheckFalse(FCountMatcher >= 2);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtMostOnce_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 1);
  CheckTrue(FCountMatcher >= 0);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtMostZero_GreaterThanOrEqualOne_False;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 0);
  CheckFalse(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.AtMostZero_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(Undefined, 0);
  CheckTrue(FCountMatcher >= 0);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.BetweenOneAndTwo_GreaterThanOrEqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckTrue(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.BetweenOneAndTwo_GreaterThanOrEqualThree_False;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckFalse(FCountMatcher >= 3);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.BetweenOneAndTwo_GreaterThanOrEqualTwo_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckTrue(FCountMatcher >= 2);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.BetweenOneAndTwo_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 2);
  CheckTrue(FCountMatcher >= 0);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.ExactlyOnce_GreaterThanOrEqualOne_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 1);
  CheckTrue(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.ExactlyOnce_GreaterThanOrEqualTwo_False;
begin
  FCountMatcher := TCountMatcher.Create(1, 1);
  CheckFalse(FCountMatcher >= 2);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.ExactlyOnce_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(1, 1);
  CheckTrue(FCountMatcher >= 0);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.ExactlyZero_GreaterThanOrEqualOne_False;
begin
  FCountMatcher := TCountMatcher.Create(0, 0);
  CheckFalse(FCountMatcher >= 1);
end;

procedure TCountMatcherGreaterThanOrEqualTestCase.ExactlyZero_GreaterThanOrEqualZero_True;
begin
  FCountMatcher := TCountMatcher.Create(0, 0);
  CheckTrue(FCountMatcher >= 0);
end;

initialization
  RegisterTest('DSharp.Testing.Mock.Internals', TCountMatcherEqualTestCase.Suite);
  RegisterTest('DSharp.Testing.Mock.Internals', TCountMatcherGreaterThanOrEqualTestCase.Suite);

end.
