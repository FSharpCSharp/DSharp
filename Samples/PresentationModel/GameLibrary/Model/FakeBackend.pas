unit FakeBackend;

interface

uses
  SysUtils,
  Interfaces,
  Forms,
  DSharp.PresentationModel,
  DSharp.Collections,
  DSharp.Collections.Extensions,
  GameDTO,
  SearchResult;

type

  [PartCreationPolicy(TCreationPolicy.cpShared)]
  TFakeBackend = class(TInterfacedObject, IBackend)
  private
    FGames: IList<TGameDTO>;
    procedure FillList;
  public
    constructor Create;
    procedure AddGameToLibrary(Title, Notes: string; Rating: Double);
    procedure CheckGameIn(Id: TGUID);
    procedure CheckGameOut(Id: TGUID; Borrower: string);
    function GetGame(Id: TGUID): TGameDTO;
    function SearchGames(SearchText: string): IEnumerable<TSearchResult>;
  end;

implementation

uses
  StrUtils;

{ TFakeBackend }

procedure TFakeBackend.AddGameToLibrary(Title, Notes: string; Rating: Double);
var
  LGame: TGameDTO;
begin
  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := Title;
  LGame.Rating := Rating;
  LGame.Notes := Notes;
  LGame.AddedOn := Now;
  FGames.Add(LGame);
end;

procedure TFakeBackend.CheckGameIn(Id: TGUID);
var
  LGame: TGameDTO;
begin
  LGame := Enumerable<TGameDTO>(FGames).FirstOrDefault(
    function(Value: TGameDTO): Boolean
    begin
      Result := Value.Id = Id;
    end);
  if Assigned(LGame) then
  begin
    LGame.Borrower := '';
  end;
end;

procedure TFakeBackend.CheckGameOut(Id: TGUID; Borrower: string);
var
  LGame: TGameDTO;
begin
  LGame := Enumerable<TGameDTO>(FGames).FirstOrDefault(
    function(Value: TGameDTO): Boolean
    begin
      Result := Value.Id = Id;
    end);
  if Assigned(LGame) then
  begin
    LGame.Borrower := Borrower;
  end;
end;

constructor TFakeBackend.Create;
begin
  FGames := TObjectList<TGameDTO>.Create;
  FillList;
end;

procedure TFakeBackend.FillList;
var
  LGame: TGameDTO;
begin
  // The 'Notes' are taken from the Game Spot reviews for each of these games.
  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := 'Halo 1';
  LGame.Rating := 1;
  LGame.Notes :=
    'Not only is this easily the best of the Xbox launch games, but it''s easily one of the best shooters ever, on any platform';
  LGame.AddedOn := Date;
  FGames.Add(LGame);

  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := 'Halo 2';
  LGame.Rating := 0.8;
  LGame.Notes :=
    'Despite a rather short campaign and a disappointing storyline, Halo 2 is an exceptional shooter that frequently delivers thrilling, memorable, and unique moments in its online, co-op, and single-player modes.';
  LGame.AddedOn := Date;
  FGames.Add(LGame);

  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := 'Halo 3';
  LGame.Rating := 1;
  LGame.Notes :=
    'Halo 3 builds upon the concepts of Halo 2 in ways that you''d expect, but there are also new modes and options that send the series in exciting new directions.';
  LGame.AddedOn := Date;
  FGames.Add(LGame);

  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := 'Mass Effect 1';
  LGame.Rating := 0.8;
  LGame.Notes :=
    'An excellent story and fun battles make this a universe worth exploring.';
  LGame.AddedOn := Date;
  FGames.Add(LGame);

  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := 'Mass Effect 2';
  LGame.Rating := 1;
  LGame.Notes :=
    'Once this intense and action-packed role-playing game pulls you into its orbit, you won''t want to escape.';
  LGame.AddedOn := Date;
  FGames.Add(LGame);

  LGame := TGameDTO.Create;
  LGame.Id := TGUID.NewGuid;
  LGame.Title := 'Final Fantasy XIII';
  LGame.Rating := 0.8;
  LGame.Notes :=
    'The most beautiful Final Fantasy game yet is an imperfect but still impressive saga that will touch your heart.';
  LGame.AddedOn := Date;
  FGames.Add(LGame);
end;

function TFakeBackend.GetGame(Id: TGUID): TGameDTO;
begin
  Result := Enumerable<TGameDTO>(FGames).FirstOrDefault(
    function(Value: TGameDTO): Boolean
    begin
      Result := Value.Id = Id;
    end);
end;

function TFakeBackend.SearchGames(SearchText: string)
  : IEnumerable<TSearchResult>;
var
  i: Integer;
  LGame: TGameDTO;
  LResult: TSearchResult;
  LItems: IList<TSearchResult>;
begin
  for i := 0 to 10 do
  begin
    Sleep(100); // Simulating network
    Application.ProcessMessages;
  end;

  LItems := TObjectList<TSearchResult>.Create;
  for LGame in FGames do
  begin
    if ContainsText(LGame.Title, SearchText) then
    begin
      LResult := TSearchResult.Create;
      LResult.Id := LGame.Id;
      LResult.Title := LGame.Title;
      LItems.Add(LResult);
    end;
  end;
  Result := LItems;
end;

initialization

TFakeBackend.ClassName;

end.
