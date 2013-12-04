unit ResultsViewModel;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  Interfaces,
  SearchResult,
  IndividualResultViewModel,
  DSharp.Collections,
  DSharp.Collections.ObservableCollection;

type
  TResultsViewModel = class(TScreen, IResultsViewModel)
  private
    FResults: IList<IIndividualResultViewModel>;
    function GetResults: IList;
    function GetStatusMessage: string;
  public
    constructor Create; override;
    function OpenWith(SearchResults: IEnumerable<TSearchResult>)
      : IResultsViewModel;
    property StatusMessage: string read GetStatusMessage;
    property Results: IList read GetResults;
  end;

implementation

uses
  DSharp.PresentationModel.BindableCollection;

constructor TResultsViewModel.Create;
begin
  inherited;
  FResults := TBindableCollection<IIndividualResultViewModel>.Create;
end;

function TResultsViewModel.GetResults: IList;
begin
  Result := FResults.AsList;
end;

function TResultsViewModel.GetStatusMessage: string;
begin
  if Results.Count = 1 then
    Result := '1 Match Found'
  else
    Result := IntToStr(Results.Count) + ' Matches Found';
end;

function TResultsViewModel.OpenWith(SearchResults: IEnumerable<TSearchResult>)
  : IResultsViewModel;
var
  LNumber: integer;
  LResult: TSearchResult;
  LItem: TIndividualResultViewModel;
begin
  Results.Clear;

  LNumber := 1;

  for LResult in SearchResults do
  begin
    LItem := TIndividualResultViewModel.Create(LResult, LNumber);
    Results.Add(LItem);
    Inc(LNumber);
  end;

  NotifyOfPropertyChange('StatusMessage');
  Result := Self;
end;

initialization

TResultsViewModel.ClassName;

end.
