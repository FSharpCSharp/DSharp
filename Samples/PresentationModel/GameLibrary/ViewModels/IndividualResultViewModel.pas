unit IndividualResultViewModel;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  Interfaces,
  SearchResult,
  DSharp.Collections.Iterators;

type
  TIndividualResultViewModel = class(TScreen, IIndividualResultViewModel)
  private
    FResult: TSearchResult;
    FNumber: integer;
    FBackend: IBackend;
    function GetTitle: string;
  public
    constructor Create(AResult: TSearchResult; ANumber: integer);
    function Open: IResult;
    property Number: integer read FNumber;
    property Title: string read GetTitle;
  end;

implementation

uses
  GameDTO,
  Show,
  ExploreGameViewModel;

{ TIndividualResultViewModel }

constructor TIndividualResultViewModel.Create(AResult: TSearchResult;
  ANumber: integer);
begin
  inherited Create;
  FResult := AResult;
  FNumber := ANumber;
  FBackend := IoC.Get<IBackend>;
end;

function TIndividualResultViewModel.GetTitle: string;
begin
  Result := FResult.Title;
end;

function TIndividualResultViewModel.Open: IResult;
var
  LGame: TGameDTO;
begin
  LGame := FBackend.GetGame(FResult.Id);
  Result := TShow.Child<TExploreGameViewModel>.Inside<IShellViewModel>.
    Configured(
    procedure(x: TExploreGameViewModel)
    begin
      x.WithGame(LGame);
    end);
end;

initialization

TIndividualResultViewModel.ClassName;

end.
