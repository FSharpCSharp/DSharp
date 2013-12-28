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
  end;

implementation

procedure TAppViewModelTestCase.SetUp;
begin
  FAppViewModel := TAppViewModel.Create();
end;

procedure TAppViewModelTestCase.TearDown;
begin
  FAppViewModel := nil;
end;

initialization
  RegisterTest(TAppViewModelTestCase.Suite);
end.
