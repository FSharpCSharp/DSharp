unit MainViewModel;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.ViewModelBase,
  Interfaces;

type
  TMainViewModel = class(TViewModelBase, IMainViewModel)
  private
    FNavigation: INavigationViewModel;
    FWorkingAreas: TArray<IWorkingAreaViewModel>;
    procedure SetWorkingAreas(const Value: TArray<IWorkingAreaViewModel>);
    function GetWorkingArea: IWorkingAreaViewModel;
    procedure SetWorkingArea(const Value: IWorkingAreaViewModel);
    procedure SetNavigation(const Value: INavigationViewModel);
  public
    [Import]
    property Navigation: INavigationViewModel read FNavigation write SetNavigation;
    [Import]
    property WorkingAreas: TArray<IWorkingAreaViewModel> read FWorkingAreas write SetWorkingAreas;

    property WorkingArea: IWorkingAreaViewModel read GetWorkingArea write SetWorkingArea;
  end;

implementation

uses
  DSharp.Bindings,
  DSharp.Collections;

{ TMainViewModel }

function TMainViewModel.GetWorkingArea: IWorkingAreaViewModel;
begin
  if Assigned(FNavigation) then
    Result := FNavigation.SelectedElement;
end;

procedure TMainViewModel.SetNavigation(const Value: INavigationViewModel);
begin
  FNavigation := Value;
//  TBinding.Create(FNavigation as TObject, 'SelectedElement', Self, 'WorkingArea');
end;

procedure TMainViewModel.SetWorkingArea(const Value: IWorkingAreaViewModel);
begin

end;

procedure TMainViewModel.SetWorkingAreas(const Value: TArray<IWorkingAreaViewModel>);
begin
  FWorkingAreas := Value;

  if Assigned(FNavigation) then
  begin
    FNavigation.Elements := TList<IWorkingAreaViewModel>.Create;
    FNavigation.Elements.AddRange(FWorkingAreas);
  end;
end;

initialization
  TMainViewModel.ClassName;

end.
