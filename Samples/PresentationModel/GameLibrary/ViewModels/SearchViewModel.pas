unit SearchViewModel;

interface

uses
  Classes,
  DSharp.Collections,
  DSharp.PresentationModel,
  Interfaces,
  NoResultsViewModel,
  ResultsViewModel,
  SearchResult;

type
  ///	<summary>
  ///	  Implementation of <see cref="ISearchViewModel" />
  ///	</summary>
  TSearchViewModel = class(TScreen, ISearchViewModel)
  private
    FNoResults: INoResultsViewModel;
    FResults: IResultsViewModel;
    FSearchResults: IScreen;
    FSearchText: string;
    FSearchGames: IEnumerable<TSearchResult>;
    FBackend: IBackend;
    function GetCanExecuteSearch: Boolean;
    procedure SetSearchResults(const Value: IScreen);
    procedure SetSearchText(const Value: string);
    procedure ExecuteSearchByCode;
  protected
    procedure OnActivate; override;
  public
    constructor Create(Backend: IBackend);
    { Public declarations }
    destructor Destroy; override;
    function AddGame: IResult;
    function ExecuteSearch: IEnumerable<IResult>;
    procedure SearchTextKeyPress(Sender: TObject; var Key: Char);
    procedure SearchTextKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    property CanExecuteSearch: Boolean read GetCanExecuteSearch;
    property SearchResults: IScreen read FSearchResults write SetSearchResults;
    property SearchText: string read FSearchText write SetSearchText;
  end;

implementation

uses
  AddGameViewModel,
  Show,
  SysUtils,
  DSharp.Collections.Iterators,
  ExploreGameViewModel,
  GameDTO,
  DSharp.Collections.Extensions;

{ TSearchViewModel }

constructor TSearchViewModel.Create(Backend: IBackend);
begin
  inherited Create;
  FNoResults := TNoResultsViewModel.Create;
  FResults := TResultsViewModel.Create;
  FBackend := Backend;
end;

{ TSearchViewModel }

destructor TSearchViewModel.Destroy;
begin

  inherited;
end;

function TSearchViewModel.AddGame: IResult;
begin
  Result := TShow.Child<IAddGameViewModel>.Inside<IShellViewModel>.Configured(
    procedure(x: IAddGameViewModel)
    begin
      x.Title := 'New Game';
    end);
end;

function TSearchViewModel.ExecuteSearch: IEnumerable<IResult>;
begin
  Result := TIteratorBlock<IResult>.Create(
    procedure
    var
      LGame: TGameDTO;
    begin
      // Show busy screen
      Yield<IResult>(TShow.Busy());

      // Search for games
      FSearchGames := FBackend.SearchGames(SearchText);

      if FSearchGames.Count = 0 then
      begin
        // Show no results screen
        SearchResults := FNoResults.WithTitle(SearchText)
      end
      else if FSearchGames.Count = 1 then
      begin
        // Get the game
        LGame := FBackend.GetGame(Enumerable<TSearchResult>(FSearchGames)
          .First.Id);

        // Show explore game screen
        Yield<IResult>(TShow.Child<IExploreGameViewModel>()
          .Inside<IShellViewModel>().Configured(
          procedure(x: IExploreGameViewModel)
          begin
            x.WithGame(LGame);
          end));
      end
      else
      begin
        // Show all results screen
        SearchResults := FResults.OpenWith(FSearchGames);
      end;

      // Hide busy screen
      Yield<IResult>(TShow.NotBusy());
    end);
end;

function TSearchViewModel.GetCanExecuteSearch: Boolean;
begin
  Result := Trim(SearchText) <> EmptyStr;
end;

procedure TSearchViewModel.OnActivate;
begin
  SearchText := '';
  SearchResults := nil;
  inherited;
end;

procedure TSearchViewModel.SetSearchResults(const Value: IScreen);
begin
  FSearchResults := Value;
  NotifyOfPropertyChange('SearchResults');
end;

procedure TSearchViewModel.SetSearchText(const Value: string);
begin
  FSearchText := Value;
  NotifyOfPropertyChange('SearchText');
  NotifyOfPropertyChange('CanExecuteSearch');
end;

procedure TSearchViewModel.ExecuteSearchByCode;
begin
  // Direct search for 'halo 1' in fmx triggers destruction from handler
  Execute.QueueActionOnUIThread(
    procedure
    var
      LContext: IActionExecutionContext;
    begin
      LContext := TActionExecutionContext.Create;
      LContext.View := GetView as TComponent;
      // Execute coroutine by code
      TCoroutine.BeginExecute(ExecuteSearch.GetEnumerator, LContext);
    end);
end;

procedure TSearchViewModel.SearchTextKeyDown(Sender: TObject; var Key: Word;
var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    Key := 0;
    ExecuteSearchByCode;
  end;
end;

procedure TSearchViewModel.SearchTextKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ExecuteSearchByCode;
  end;
end;

initialization

TSearchViewModel.ClassName;

end.
