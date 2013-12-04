unit InterestItemViewModel;

interface

uses
  Classes,
  SysUtils,
  Interfaces,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.PresentationModel.EventAggregator,
  DSharp.PresentationModel.ConductorWithCollectionOneActive,
  InterestSelectorViewModel;

type
  ///	<summary>
  ///	  Implementation of <see cref="IInterestViewModel" />
  ///	</summary>
  TInterestItemViewModel = class(TScreen, IInterestItemViewModel)
  private
    FIsSelected: Boolean;
    FParentSelector: TInterestSelectorViewModel;
    function GetIsSelected: Boolean;
    procedure SetIsSelected(const Value: Boolean);
  public
    { Public declarations }
    constructor Create(Name: string;
      ParentSelector: TInterestSelectorViewModel);
    property IsSelected: Boolean read GetIsSelected write SetIsSelected;
    property ParentSelector: TInterestSelectorViewModel read FParentSelector;
  end;

implementation

constructor TInterestItemViewModel.Create(Name: string;
  ParentSelector: TInterestSelectorViewModel);
begin
  inherited Create;

  DisplayName := Name;
  FParentSelector := ParentSelector;
end;

function TInterestItemViewModel.GetIsSelected: Boolean;
begin
  Result := FIsSelected;
end;

procedure TInterestItemViewModel.SetIsSelected(const Value: Boolean);
begin
  if FIsSelected <> Value then
  begin
    FIsSelected := Value;
    NotifyOfPropertyChange('IsSelected');
    ParentSelector.OnInterestSelectionChanged;
  end;
end;

initialization

TInterestItemViewModel.ClassName;

end.
