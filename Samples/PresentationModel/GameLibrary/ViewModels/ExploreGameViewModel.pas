unit ExploreGameViewModel;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  Interfaces,
  GameDTO;

type
  TExploreGameViewModel = class(TScreen, IExploreGameViewModel)
  private
    FBorrower: string;
    FGame: TGameDTO;
    [Import]
    FBackend: IBackend;
    function GetBorrowedMessage: string;
    function GetCanCheckOut: Boolean;
    function GetGame: TGameDTO;
    function GetIsCheckedIn: Boolean;
    function GetIsCheckedOut: Boolean;
    procedure SetBorrower(const Value: string);
    procedure SetBorrowerInternal(const Value: string);
  public
    procedure WithGame(Game: TGameDTO);
    procedure CheckIn;
    procedure CheckOut;
    property BorrowedMessage: string read GetBorrowedMessage;
    property Borrower: string read FBorrower write SetBorrower;
    property CanCheckOut: Boolean read GetCanCheckOut;
    property Game: TGameDTO read GetGame;
    property IsCheckedIn: Boolean read GetIsCheckedIn;
    property IsCheckedOut: Boolean read GetIsCheckedOut;
  end;

implementation

procedure TExploreGameViewModel.CheckIn;
begin
  SetBorrowerInternal('');
  FBackend.CheckGameIn(FGame.Id);
end;

procedure TExploreGameViewModel.CheckOut;
begin
  SetBorrowerInternal(Borrower);
  FBackend.CheckGameOut(FGame.Id, FGame.Borrower);
end;

function TExploreGameViewModel.GetBorrowedMessage: string;
begin
  Result := Game.Title + ' is currently checked out to ' + Game.Borrower + '.';
end;

function TExploreGameViewModel.GetCanCheckOut: Boolean;
begin
  Result := Trim(Borrower) <> '';
end;

function TExploreGameViewModel.GetGame: TGameDTO;
begin
  Result := FGame;
end;

function TExploreGameViewModel.GetIsCheckedIn: Boolean;
begin
  Result := not IsCheckedOut;
end;

function TExploreGameViewModel.GetIsCheckedOut: Boolean;
begin
  Result := Trim(Game.Borrower) <> '';
end;

procedure TExploreGameViewModel.SetBorrower(const Value: string);
begin
  FBorrower := Value;
  NotifyOfPropertyChange('Borrower');
  NotifyOfPropertyChange('CanCheckOut');
end;

procedure TExploreGameViewModel.SetBorrowerInternal(const Value: string);
begin
  FGame.Borrower := Value;
  Borrower := Value;
  NotifyOfPropertyChange('IsCheckedOut');
  NotifyOfPropertyChange('IsCheckedIn');
  NotifyOfPropertyChange('BorrowedMessage');
end;

procedure TExploreGameViewModel.WithGame(Game: TGameDTO);
begin
  Assert(Assigned(Game));
  FGame := Game;
end;

initialization

TExploreGameViewModel.ClassName;

end.
