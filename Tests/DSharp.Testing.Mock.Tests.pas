unit DSharp.Testing.Mock.Tests;

interface

uses
  TestFramework, DSharp.Testing.Mock;

type
  {$M+}
  ITest = interface
    function ReturnBoolean: Boolean;
  end;

  TMockTestCase = class(TTestCase)
  private
    FMock: Mock<ITest>;
    FTest: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WillReturn_DifferentExpectations;
    procedure WillExecute_InSequence;
  end;

implementation

uses
  DSharp.Testing.Mock.Sequences;

{ TMockTestCase }

procedure TMockTestCase.WillExecute_InSequence;
var
  seq: Sequence;
begin
  FMock.WillExecute.InSequence(seq).Exactly(4).WhenCalling.ReturnBoolean;
  FMock.WillExecute.InSequence(seq).Once.WhenCalling.ReturnBoolean;

  FTest.ReturnBoolean;
  FTest.ReturnBoolean;
  FTest.ReturnBoolean;
  FTest.ReturnBoolean;
//  FTest.ReturnBoolean;
  FTest.ReturnBoolean;
  seq.Verify;
end;

procedure TMockTestCase.WillReturn_DifferentExpectations;
begin
  FMock.WillReturn(True).Once.WhenCalling.ReturnBoolean;
  FMock.WillReturn(False).Once.WhenCalling.ReturnBoolean;

  CheckTrue(FTest.ReturnBoolean);
  CheckFalse(FTest.ReturnBoolean);
end;

procedure TMockTestCase.SetUp;
begin
  FTest := FMock;
end;

procedure TMockTestCase.TearDown;
begin
  FTest := nil;
  FMock.Free;
end;

initialization
  RegisterTest('DSharp.Testing.Mock', TMockTestCase.Suite);

end.
