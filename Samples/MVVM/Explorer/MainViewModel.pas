unit MainViewModel;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.ViewModelBase,
  Interfaces;

type
  TMainViewModel = class(TViewModelBase, IMainViewModel)
  private
    [Import]
    FNavigation: INavigationViewModel;
    FWorkingArea: IWorkingAreaViewModel;
  public
    property Navigation: INavigationViewModel read FNavigation write FNavigation;
    [Import]
    property WorkingArea: IWorkingAreaViewModel read FWorkingArea write FWorkingArea;
  end;

implementation

initialization
  TMainViewModel.ClassName;

end.
