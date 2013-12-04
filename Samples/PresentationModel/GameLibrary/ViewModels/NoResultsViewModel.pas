unit NoResultsViewModel;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  Interfaces;

type
  TNoResultsViewModel = class(TScreen, INoResultsViewModel)
  private
    FSearchText: string;
  public
    function WithTitle(SearchText: string): INoResultsViewModel;
    function AddGame: IResult;
  end;

implementation

uses
  Show;

{ TNoResultsViewModel }

function TNoResultsViewModel.AddGame: IResult;
begin
  Result := TShow.Child<IAddGameViewModel>().Inside<IShellViewModel>()
    .Configured(
    procedure(x: IAddGameViewModel)
    begin
      x.Title := FSearchText;
    end);
end;

function TNoResultsViewModel.WithTitle(SearchText: string): INoResultsViewModel;
begin
  FSearchText := SearchText;
  Result := Self;
end;

initialization

TNoResultsViewModel.ClassName;

end.
