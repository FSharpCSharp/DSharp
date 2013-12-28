unit AppViewModel;

interface

uses
  AppInterfaces,
  DSharp.PresentationModel;

type
  TAppViewModel = class(TScreen, IAppViewModel)
  public
    constructor Create(); override;
  end;

implementation

constructor TAppViewModel.Create();
begin
  inherited Create();
  DisplayName := IAppViewModel_DisplayName;
end;

initialization
  TAppViewModel.ClassName;
end.
