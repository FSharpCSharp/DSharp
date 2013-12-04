unit InterestSelectorViewModel;

interface

uses
  Classes,
  SysUtils,
  Interfaces,
  DSharp.Core.Events,
  DSharp.Validation,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.PresentationModel.EventAggregator,
  DSharp.PresentationModel.ConductorWithCollectionOneActive;

type
  ///	<summary>
  ///	  Implementation of <see cref="IInterestSelectorViewModel" />
  ///	</summary>
  TInterestSelectorViewModel = class(TValidatingScreen,
    IInterestSelectorViewModel)
  private
    { Private declarations }
    FInterests: IList<IInterestItemViewModel>;
    FSelectedInterestsChanged: Event<TNotifyEvent>;
    function GetSelectedInterests: IList<IInterestItemViewModel>;
    function GetSelectedInterestsChanged: IEvent<TNotifyEvent>;
    procedure OnSelectedInterestsChanged;
  public
    { Public declarations }
    constructor Create; override;
    procedure ConfigureValidationRules;
    procedure OnInterestSelectionChanged;
    property Interests: IList<IInterestItemViewModel> read FInterests;
    property SelectedInterests: IList<IInterestItemViewModel>
      read GetSelectedInterests;
    property SelectedInterestsChanged: IEvent<TNotifyEvent>
      read GetSelectedInterestsChanged;
  end;

implementation

uses
  InterestItemViewModel,
  DSharp.PresentationModel.BindableCollection;

constructor TInterestSelectorViewModel.Create;
begin
  inherited Create;

  FInterests := TBindableCollection<IInterestItemViewModel>.Create;
  FInterests.Add(TInterestItemViewModel.Create('Music', Self));
  FInterests.Add(TInterestItemViewModel.Create('Movies', Self));
  FInterests.Add(TInterestItemViewModel.Create('Sports', Self));
  FInterests.Add(TInterestItemViewModel.Create('Shopping', Self));
  FInterests.Add(TInterestItemViewModel.Create('Hunting', Self));
  FInterests.Add(TInterestItemViewModel.Create('Books', Self));
  FInterests.Add(TInterestItemViewModel.Create('Physics', Self));
  FInterests.Add(TInterestItemViewModel.Create('Comics', Self));

  ConfigureValidationRules;
end;

procedure TInterestSelectorViewModel.ConfigureValidationRules;
begin
  Validator.AddRule('Password',
    function: TRuleResult
    begin
      Result := TRuleResult.Assert(SelectedInterests.Count >= 3,
        'Password must contain at least 6 characters');
    end);
end;

function TInterestSelectorViewModel.GetSelectedInterests
  : IList<IInterestItemViewModel>;
var
  LItem: IInterestItemViewModel;
begin
  Result := TList<IInterestItemViewModel>.Create;
  for LItem in Interests do
    if LItem.IsSelected then
      Result.Add(LItem);
end;

function TInterestSelectorViewModel.GetSelectedInterestsChanged
  : IEvent<TNotifyEvent>;
begin
  Result := FSelectedInterestsChanged;
end;

procedure TInterestSelectorViewModel.OnInterestSelectionChanged;
begin
  OnSelectedInterestsChanged;
end;

procedure TInterestSelectorViewModel.OnSelectedInterestsChanged;
begin
  SelectedInterestsChanged.Invoke(Self);
end;

initialization

TInterestSelectorViewModel.ClassName;

end.
