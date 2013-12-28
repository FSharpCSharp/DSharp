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
    procedure Test_DisplayName();
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

procedure TAppViewModelTestCase.Test_DisplayName();
var
  LHaveDisplayName: IHaveDisplayName;
begin
  LHaveDisplayName := AppViewModel as IHaveDisplayName;
  CheckEquals(IAppViewModel_DisplayName, LHaveDisplayName.DisplayName);
end;

initialization
  RegisterTest(TAppViewModelTestCase.Suite);
end.
