unit AppViewModelTestCase;

interface

uses
  TestFramework,
  AppInterfaces,
  AppViewModel;

type
  TAppViewModelTestCase = class(TTestCase)
  strict private
    FAppViewModel: IAppViewModel;
  strict protected
    property AppViewModel: IAppViewModel read FAppViewModel;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_DecrementCount_MaximumCount();
    procedure Test_DecrementCount_MaximumCount_Minus1();
    procedure Test_DecrementCount_MaximumCount_Plus1();
    procedure Test_DecrementCount_MinimumCount();
    procedure Test_DecrementCount_MinimumCount_Minus1();
    procedure Test_DecrementCount_MinimumCount_Plus1();
    procedure Test_DisplayName();
    procedure Test_IncrementCount_MaximumCount();
    procedure Test_IncrementCount_MaximumCount_Minus1();
    procedure Test_IncrementCount_MaximumCount_Plus1();
    procedure Test_IncrementCount_MinimumCount();
    procedure Test_IncrementCount_MinimumCount_Minus1();
    procedure Test_IncrementCount_MinimumCount_Plus1();
  end;

implementation

uses
  DSharp.PresentationModel;

procedure TAppViewModelTestCase.SetUp;
begin
  FAppViewModel := TAppViewModel.Create();
end;

procedure TAppViewModelTestCase.TearDown;
begin
  FAppViewModel := nil;
end;

procedure TAppViewModelTestCase.Test_DecrementCount_MaximumCount();
begin
  AppViewModel.Count := MaximumCount;
  AppViewModel.DecrementCount();
end;

procedure TAppViewModelTestCase.Test_DecrementCount_MaximumCount_Minus1();
begin
  AppViewModel.Count := MaximumCount-1;
  AppViewModel.DecrementCount();
end;

procedure TAppViewModelTestCase.Test_DecrementCount_MaximumCount_Plus1();
begin
  AppViewModel.Count := MaximumCount+1;
  AppViewModel.DecrementCount();
end;

procedure TAppViewModelTestCase.Test_DecrementCount_MinimumCount();
begin
  AppViewModel.Count := MinimumCount;
  AppViewModel.DecrementCount();
end;

procedure TAppViewModelTestCase.Test_DecrementCount_MinimumCount_Minus1();
begin
  AppViewModel.Count := MinimumCount-1;
  AppViewModel.DecrementCount();
end;

procedure TAppViewModelTestCase.Test_DecrementCount_MinimumCount_Plus1();
begin
  AppViewModel.Count := MinimumCount+1;
  AppViewModel.DecrementCount();
end;

procedure TAppViewModelTestCase.Test_DisplayName();
var
  LHaveDisplayName: IHaveDisplayName;
begin
  LHaveDisplayName := AppViewModel as IHaveDisplayName;
  CheckEquals(IAppViewModel_DisplayName, LHaveDisplayName.DisplayName);
end;

procedure TAppViewModelTestCase.Test_IncrementCount_MaximumCount();
begin
  AppViewModel.Count := MaximumCount;
  AppViewModel.IncrementCount();
end;

procedure TAppViewModelTestCase.Test_IncrementCount_MaximumCount_Minus1();
begin
  AppViewModel.Count := MaximumCount-1;
  AppViewModel.IncrementCount();
end;

procedure TAppViewModelTestCase.Test_IncrementCount_MaximumCount_Plus1();
begin
  AppViewModel.Count := MaximumCount+1;
  AppViewModel.IncrementCount();
end;

procedure TAppViewModelTestCase.Test_IncrementCount_MinimumCount();
begin
  AppViewModel.Count := MinimumCount;
  AppViewModel.IncrementCount();
end;

procedure TAppViewModelTestCase.Test_IncrementCount_MinimumCount_Minus1();
begin
  AppViewModel.Count := MinimumCount-1;
  AppViewModel.IncrementCount();
end;

procedure TAppViewModelTestCase.Test_IncrementCount_MinimumCount_Plus1();
begin
  AppViewModel.Count := MinimumCount+1;
  AppViewModel.IncrementCount();
end;

initialization
  RegisterTest(TAppViewModelTestCase.Suite);
end.
