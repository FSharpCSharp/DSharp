unit AppViewModel;

interface

uses
  Sysutils,
  StdCtrls,
  AppInterfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ViewSettings;

type
  TAppViewModel = class(TScreen, IAppViewModel)
  strict private
    FDescription: string;
    [Import]
    FWindowManager: IWindowManager;
  private
    procedure ExecuteDialogViewModel(const LMyDialogViewModel
      : IMyDialogViewModel; const LMyDialogViewSettings: IViewSettings = nil);
  strict protected
    procedure SetDescription(const Value: string); virtual;
  public
    constructor Create; override;
    procedure ActionClose;
    procedure ActionOpen1;
    procedure ActionOpen2;
    property Description: string read FDescription write SetDescription;
    property WindowManager: IWindowManager read FWindowManager;
  end;

implementation

uses
  Graphics,
  Forms,
  MyDialogViewModel;

constructor TAppViewModel.Create;
begin
  inherited;

  // Initialize applications title
  DisplayName := 'How to ActionOpen1 Dialog';

  // Initialize Description property
  Description := 'What''s up?';
end;

procedure TAppViewModel.ActionClose;
begin
  TryClose();
end;

procedure TAppViewModel.ActionOpen1;
var
  LMyDialogViewModel: IMyDialogViewModel;
begin
  // Example1: Create dialog manually (but this way we introduce dependency on unit MyDialogViewModel)
  LMyDialogViewModel := TMyDialogViewModel.Create();

  ExecuteDialogViewModel(LMyDialogViewModel);
end;

procedure TAppViewModel.ActionOpen2;
var
  LMyDialogViewModel: IMyDialogViewModel;
  LMyDialogViewSettings: IViewSettings;
begin
  // Example2: Use IoC to provide dialog
  LMyDialogViewModel := IoC.Get<IMyDialogViewModel>();

  // Create custom view settings (these properties will be applied to the dialog form)
  LMyDialogViewSettings := TViewSettings.Create();
  LMyDialogViewSettings['BorderStyle'] := TValue.From(bsSizeable);
  LMyDialogViewSettings['Color'] := TValue.From(clSkyBlue);

  ExecuteDialogViewModel(LMyDialogViewModel, LMyDialogViewSettings);
end;

procedure TAppViewModel.ExecuteDialogViewModel(const LMyDialogViewModel
  : IMyDialogViewModel; const LMyDialogViewSettings: IViewSettings = nil);
begin
  // Set display name in a generic way
  (LMyDialogViewModel as IHaveDisplayName).DisplayName := 'Enter description:';

  // Set initial Description for the dialog
  LMyDialogViewModel.Value := Description;

  // find the View for IMyDialogViewModel,
  // then builds a modal dialog around the View,
  // then applies the supplied settings (`nil` means no settings),
  // then show that dialog modally, and checks the modal result.
  if WindowManager.ShowDialog(LMyDialogViewModel, nil, LMyDialogViewSettings) = mrOk
  then
  begin
    // Read new Description from the dialog
    Description := LMyDialogViewModel.Value;
  end;
end;

procedure TAppViewModel.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    NotifyOfPropertyChange('Description');
  end;
end;

initialization

TAppViewModel.ClassName;

end.
