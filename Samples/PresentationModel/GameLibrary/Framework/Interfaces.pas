unit Interfaces;

interface

uses
  DSharp.Collections,
  DSharp.PresentationModel,
  GameDTO,
  SearchResult;

type

  [InheritedExport]
  IBackend = interface
    ['{F3586720-50FB-47F3-906E-D061F832EB52}']
    procedure AddGameToLibrary(Title, Notes: string; Rating: Double);
    procedure CheckGameIn(Id: TGUID);
    procedure CheckGameOut(Id: TGUID; Borrower: string);
    function GetGame(Id: TGUID): TGameDTO;
    function SearchGames(SearchText: string): IEnumerable<TSearchResult>;
  end;

  [InheritedExport]
  IShellViewModel = interface(IScreen)
    ['{42E667E6-3E76-4ABA-8D7B-06ECC475A259}']
  end;

  [InheritedExport]
  ISearchViewModel = interface(IScreen)
    ['{25E57C69-F2A1-4867-971A-A201A9D72E90}']
  end;

  [InheritedExport]
  IResultsViewModel = interface(IScreen)
    ['{5A6CD9AE-C320-4512-85CB-809E54AC2075}']
    function OpenWith(SearchResults: IEnumerable<TSearchResult>)
      : IResultsViewModel;
  end;

  [InheritedExport]
  INoResultsViewModel = interface(IScreen)
    ['{38FBA727-5A79-439F-BD2E-F20BADF7BCDF}']
    function WithTitle(SearchText: string): INoResultsViewModel;
  end;

  [InheritedExport]
  IIndividualResultViewModel = interface(IScreen)
    ['{50E4B8D4-146A-4B44-B014-2F2539D89BD3}']
  end;

  [InheritedExport]
  IAddGameViewModel = interface(IScreen)
    ['{07701497-E5FF-4C73-A346-3BD8CF76F2A6}']
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    property Title: string read GetTitle write SetTitle;
  end;

  [InheritedExport]
  IExploreGameViewModel = interface(IScreen)
    ['{150DA1FA-C4A1-4DF8-94C0-26BFCA9A65D0}']
    procedure WithGame(Game: TGameDTO);
  end;

implementation

end.
