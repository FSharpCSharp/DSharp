unit zMainViewModel;

interface

uses
  DSharp.Collections,
  DSharp.PresentationModel,
  Interfaces;

type
  TMainViewModel = class(TScreen, IMainViewModel)
  public
    function AddGame: IEnumerable<IResult>;
    function ShowGame: IResult;
  end;

implementation

uses
  DSharp.Collections.Iterators,
  ShowMessage;

{ TMainViewModel }

function TMainViewModel.AddGame: IEnumerable<IResult>;
begin
  Result := TIteratorBlock<IResult>.Create(
    procedure
    begin
      Yield<IResult>(TShowMessage.Create('Started.'));
      Yield<IResult>(TShowMessage.Create('Working...'));
      Yield<IResult>(TShowMessage.Create('Finished.'));
    end);
end;

function TMainViewModel.ShowGame: IResult;
begin
  Result := TShowMessage.Create('ShowGame.');
end;

initialization

TMainViewModel.ClassName;

end.
