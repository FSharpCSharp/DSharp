unit AppViewModelTestCase;

interface

uses
  TestFramework, DSharp.PresentationModel, AppInterfaces, AppViewModel;

type
  // Test methods for class TAppViewModel
  TAppViewModelTestCase = class(TTestCase)
  strict private
    FAppViewModel: TAppViewModel;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDecrementCount_Minus10();
    procedure TestDecrementCount_Minus11();
    procedure TestDecrementCount_Minus9();
    procedure TestDecrementCount_Plus10();
    procedure TestDecrementCount_Plus11();
    procedure TestDecrementCount_Plus9();
    procedure TestIncrementCountBy2_Minus10();
    procedure TestIncrementCountBy2_Minus11();
    procedure TestIncrementCountBy2_Minus9();
    procedure TestIncrementCountBy2_Plus10();
    procedure TestIncrementCountBy2_Plus11();
    procedure TestIncrementCountBy2_Plus9();
    procedure TestIncrementCountByIncrementValue_Minus10();
    procedure TestIncrementCountByIncrementValue_Minus11();
    procedure TestIncrementCountByIncrementValue_Minus9();
    procedure TestIncrementCountByIncrementValue_Plus10();
    procedure TestIncrementCountByIncrementValue_Plus11();
    procedure TestIncrementCountByIncrementValue_Plus9();
    procedure TestIncrementCount_Minus10();
    procedure TestIncrementCount_Minus11();
    procedure TestIncrementCount_Minus9();
    procedure TestIncrementCount_Plus11();
    procedure TestIncrementCount_Plus9();
    procedure TestIncrementCount_Plus10();
    procedure TestMultiplyCountBy2_Minus10();
    procedure TestMultiplyCountBy2_Minus11();
    procedure TestMultiplyCountBy2_Minus9();
    procedure TestMultiplyCountBy2_Plus10();
    procedure TestMultiplyCountBy2_Plus11();
    procedure TestMultiplyCountBy2_Plus9();
  end;

implementation

uses
  AppModel;

procedure TAppViewModelTestCase.SetUp;
begin
  FAppViewModel := TAppViewModel.Create(TAppModel.Create());
end;

procedure TAppViewModelTestCase.TearDown;
begin
  FAppViewModel.Free;
  FAppViewModel := nil;
end;

procedure TAppViewModelTestCase.TestDecrementCount_Minus10();
begin
  FAppViewModel.Count := -10;
  FAppViewModel.DecrementCount;
end;

procedure TAppViewModelTestCase.TestDecrementCount_Minus11();
begin
  FAppViewModel.Count := -11;
  FAppViewModel.DecrementCount;
end;

procedure TAppViewModelTestCase.TestDecrementCount_Minus9();
begin
  FAppViewModel.Count := -9;
  FAppViewModel.DecrementCount;
end;

procedure TAppViewModelTestCase.TestDecrementCount_Plus10();
begin
  FAppViewModel.Count := 10;
  FAppViewModel.DecrementCount;
end;

procedure TAppViewModelTestCase.TestDecrementCount_Plus11();
begin
  FAppViewModel.Count := 11;
  FAppViewModel.DecrementCount;
end;

procedure TAppViewModelTestCase.TestDecrementCount_Plus9();
begin
  FAppViewModel.Count := 9;
  FAppViewModel.DecrementCount;
end;

procedure TAppViewModelTestCase.TestIncrementCountBy2_Minus10();
begin
  FAppViewModel.Count := -10;
  FAppViewModel.IncrementCountBy2;
end;

procedure TAppViewModelTestCase.TestIncrementCountBy2_Minus11();
begin
  FAppViewModel.Count := -11;
  FAppViewModel.IncrementCountBy2;
end;

procedure TAppViewModelTestCase.TestIncrementCountBy2_Minus9();
begin
  FAppViewModel.Count := -9;
  FAppViewModel.IncrementCountBy2;
end;

procedure TAppViewModelTestCase.TestIncrementCountBy2_Plus10();
begin
  FAppViewModel.Count := 10;
  FAppViewModel.IncrementCountBy2;
end;

procedure TAppViewModelTestCase.TestIncrementCountBy2_Plus11();
begin
  FAppViewModel.Count := 11;
  FAppViewModel.IncrementCountBy2;
end;

procedure TAppViewModelTestCase.TestIncrementCountBy2_Plus9();
begin
  FAppViewModel.Count := 9;
  FAppViewModel.IncrementCountBy2;
end;

procedure TAppViewModelTestCase.TestIncrementCountByIncrementValue_Minus10();
begin
  FAppViewModel.Count := -10;
  FAppViewModel.IncrementValue := 5;
  FAppViewModel.IncrementCountByIncrementValue;
end;

procedure TAppViewModelTestCase.TestIncrementCountByIncrementValue_Minus11();
begin
  FAppViewModel.Count := -11;
  FAppViewModel.IncrementValue := 5;
  FAppViewModel.IncrementCountByIncrementValue;
end;

procedure TAppViewModelTestCase.TestIncrementCountByIncrementValue_Minus9();
begin
  FAppViewModel.Count := -9;
  FAppViewModel.IncrementValue := 5;
  FAppViewModel.IncrementCountByIncrementValue;
end;

procedure TAppViewModelTestCase.TestIncrementCountByIncrementValue_Plus10();
begin
  FAppViewModel.Count := 10;
  FAppViewModel.IncrementValue := 5;
  FAppViewModel.IncrementCountByIncrementValue;
end;

procedure TAppViewModelTestCase.TestIncrementCountByIncrementValue_Plus11();
begin
  FAppViewModel.Count := 11;
  FAppViewModel.IncrementValue := 5;
  FAppViewModel.IncrementCountByIncrementValue;
end;

procedure TAppViewModelTestCase.TestIncrementCountByIncrementValue_Plus9();
begin
  FAppViewModel.Count := 9;
  FAppViewModel.IncrementValue := 5;
  FAppViewModel.IncrementCountByIncrementValue;
end;

procedure TAppViewModelTestCase.TestIncrementCount_Minus10();
begin
  FAppViewModel.Count := -10;
  FAppViewModel.IncrementCount;
end;

procedure TAppViewModelTestCase.TestIncrementCount_Minus11();
begin
  FAppViewModel.Count := -11;
  FAppViewModel.IncrementCount;
end;

procedure TAppViewModelTestCase.TestIncrementCount_Minus9();
begin
  FAppViewModel.Count := -9;
  FAppViewModel.IncrementCount;
end;

procedure TAppViewModelTestCase.TestIncrementCount_Plus11();
begin
  FAppViewModel.Count := 11;
  FAppViewModel.IncrementCount;
end;

procedure TAppViewModelTestCase.TestIncrementCount_Plus9();
begin
  FAppViewModel.Count := 9;
  FAppViewModel.IncrementCount;
end;

procedure TAppViewModelTestCase.TestIncrementCount_Plus10();
begin
  FAppViewModel.Count := 10;
  FAppViewModel.IncrementCount;
end;

procedure TAppViewModelTestCase.TestMultiplyCountBy2_Minus10();
begin
  FAppViewModel.Count := -10;
  FAppViewModel.MultiplyCountBy2;
end;

procedure TAppViewModelTestCase.TestMultiplyCountBy2_Minus11();
begin
  FAppViewModel.Count := -11;
  FAppViewModel.MultiplyCountBy2;
end;

procedure TAppViewModelTestCase.TestMultiplyCountBy2_Minus9();
begin
  FAppViewModel.Count := -9;
  FAppViewModel.MultiplyCountBy2;
end;

procedure TAppViewModelTestCase.TestMultiplyCountBy2_Plus10();
begin
  FAppViewModel.Count := 10;
  FAppViewModel.MultiplyCountBy2;
end;

procedure TAppViewModelTestCase.TestMultiplyCountBy2_Plus11();
begin
  FAppViewModel.Count := 11;
  FAppViewModel.MultiplyCountBy2;
end;

procedure TAppViewModelTestCase.TestMultiplyCountBy2_Plus9();
begin
  FAppViewModel.Count := 9;
  FAppViewModel.MultiplyCountBy2;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TAppViewModelTestCase.Suite);
end.
