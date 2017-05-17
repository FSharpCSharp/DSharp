unit NavigationViewModel;

interface

uses
  DSharp.Collections,
  DSharp.PresentationModel.ViewModelBase,
  Interfaces;

type
  TNavigationViewModel = class(TViewModelBase, INavigationViewModel)
  private
    FElements: IList<IWorkingAreaViewModel>;
    FSelectedElement: IWorkingAreaViewModel;
    function GetSelectedElement: IWorkingAreaViewModel;
    function GetElements: IList<IWorkingAreaViewModel>;
    procedure SetElements(const Value: IList<IWorkingAreaViewModel>);
    procedure SetSelectedElement(const Value: IWorkingAreaViewModel);
  public
    property Elements: IList<IWorkingAreaViewModel> read GetElements write SetElements;
    property SelectedElement: IWorkingAreaViewModel read GetSelectedElement write SetSelectedElement;
  end;

implementation

{ TNavigationViewModel }

function TNavigationViewModel.GetElements: IList<IWorkingAreaViewModel>;
begin
  Result := FElements;
end;

function TNavigationViewModel.GetSelectedElement: IWorkingAreaViewModel;
begin
  Result := FSelectedElement;
end;

procedure TNavigationViewModel.SetElements(const Value: IList<IWorkingAreaViewModel>);
begin
  FElements := Value;
end;

procedure TNavigationViewModel.SetSelectedElement(const Value: IWorkingAreaViewModel);
begin
  FSelectedElement := Value;
//  DoPropertyChanged('SelectedElement');
end;

initialization
  TNavigationViewModel.ClassName;

end.
