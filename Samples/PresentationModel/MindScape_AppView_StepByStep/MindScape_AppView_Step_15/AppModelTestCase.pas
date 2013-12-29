unit AppModelTestCase;

interface

uses
  TestFramework,
  AppInterfaces;

type
  TAppModelTestCase = class(TTestCase)
  strict private
    FAppModel: IAppModel;
  strict protected
    property AppModel: IAppModel read FAppModel;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_IncrementValue_MinimumCount();
    procedure Test_IncrementValue_MinimumCount_Plus1();
    procedure Test_IncrementValue_MinimumCount_Minus1();
    procedure Test_IncrementValue_MaximumCount();
    procedure Test_IncrementValue_MaximumCount_Plus1();
    procedure Test_IncrementValue_MaximumCount_Minus1();
    procedure Test_Count_MaximumCount();
    procedure Test_Count_MaximumCount_Minus1();
    procedure Test_Count_MaximumCount_Plus1();
    procedure Test_Count_MinimumCount();
    procedure Test_Count_MinimumCount_Minus1();
    procedure Test_Count_MinimumCount_Plus1();
  end;

implementation

uses
  SysUtils,
  Classes,
  AppModel;

procedure TAppModelTestCase.SetUp;
begin
  FAppModel := TAppModel.Create();
end;

procedure TAppModelTestCase.TearDown;
begin
  FAppModel := nil;
end;

procedure TAppModelTestCase.Test_IncrementValue_MinimumCount();
begin
  AppModel.IncrementValue := MinimumCount;
  AppModel.IncrementValue := 5;
end;

procedure TAppModelTestCase.Test_IncrementValue_MinimumCount_Plus1();
begin
  AppModel.IncrementValue := MinimumCount+1;
  AppModel.IncrementValue := 5;
end;

procedure TAppModelTestCase.Test_IncrementValue_MinimumCount_Minus1();
begin
  AppModel.IncrementValue := MinimumCount-1;
  AppModel.IncrementValue := 5;
end;

procedure TAppModelTestCase.Test_IncrementValue_MaximumCount();
begin
  AppModel.IncrementValue := MaximumCount;
  AppModel.IncrementValue := 5;
end;

procedure TAppModelTestCase.Test_IncrementValue_MaximumCount_Plus1();
begin
  AppModel.IncrementValue := MaximumCount+1;
  AppModel.IncrementValue := 5;
end;

procedure TAppModelTestCase.Test_IncrementValue_MaximumCount_Minus1();
begin
  AppModel.IncrementValue := MaximumCount-1;
  AppModel.IncrementValue := 5;
end;

procedure TAppModelTestCase.Test_Count_MaximumCount();
begin
  AppModel.Count := MaximumCount;
end;

procedure TAppModelTestCase.Test_Count_MaximumCount_Minus1();
begin
  AppModel.Count := MaximumCount-1;
end;

procedure TAppModelTestCase.Test_Count_MaximumCount_Plus1();
begin
  ExpectedException := EArgumentOutOfRangeException;
  AppModel.Count := MaximumCount+1;
end;

procedure TAppModelTestCase.Test_Count_MinimumCount();
begin
  AppModel.Count := MinimumCount;
end;

procedure TAppModelTestCase.Test_Count_MinimumCount_Minus1();
begin
  ExpectedException := EArgumentOutOfRangeException;
  AppModel.Count := MinimumCount-1;
end;

procedure TAppModelTestCase.Test_Count_MinimumCount_Plus1();
begin
  AppModel.Count := MinimumCount+1;
end;

initialization
  RegisterTest(TAppModelTestCase.Suite);
end.
