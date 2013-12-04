unit AddGameViewModel;

interface

uses
  SysUtils,
  Interfaces,
  Error,
  ValidatorIntf,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.ComponentModel.DataAnnotations,
  DSharp.Core.Validations;

type

  ///	<summary>
  ///	  Implementation of <see cref="IAddGameViewModel" />
  ///	</summary>
  TAddGameViewModel = class(TScreen, IAddGameViewModel, IDataErrorInfo)
  private
    FTitle: string;
    FNotes: string;
    FRating: double;
    FWasSaved: Boolean;
    FBackend: IBackend;
    [Import]
    FWindowManager: IWindowManager;
    [Import]
    FValidator: IValidator;
    function GetCanAddGame: Boolean;
    function GetError: string;
    function GetItem(const Name: string): string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure SetNotes(const Value: string);
    procedure SetRating(const Value: double);
  public
    constructor Create(Backend: IBackend);
    function AddGame: IEnumerable<IResult>;
    procedure CanClose(Callback: TProc<Boolean>); override;
    property CanAddGame: Boolean read GetCanAddGame;
    [Required]
    property Title: string read FTitle write SetTitle;
    property Notes: string read FNotes write SetNotes;
    property Rating: double read FRating write SetRating;
  end;

implementation

uses
  DSharp.Collections.Iterators,
  OpenChildResult,
  Show;

function TAddGameViewModel.AddGame: IEnumerable<IResult>;
begin
  Result := TIteratorBlock<IResult>.Create(
    procedure
    begin
      FBackend.AddGameToLibrary(Title, Notes, Rating);
      FWasSaved := True;
      Yield<IResult>(TShow.Child<ISearchViewModel>().Inside<IShellViewModel>());
    end);
end;

procedure TAddGameViewModel.CanClose(Callback: TProc<Boolean>);
begin
  Callback(FWasSaved or
    (FWindowManager.MessageDlg
    ('Are you sure you want to cancel?  Changes will be lost.', mtConfirmation,
    mbOKCancel, 0) = mrOk));
end;

constructor TAddGameViewModel.Create(Backend: IBackend);
begin
  inherited Create;
  FBackend := Backend;
end;

function TAddGameViewModel.GetCanAddGame: Boolean;
begin
  Result := Trim(GetError) = '';
end;

function TAddGameViewModel.GetError: string;
var
  LError: TError;
begin
  Result := '';
  for LError in FValidator.Validate(Self) do
    Result := Result + LError.ErrorMessage;
end;

function TAddGameViewModel.GetItem(const Name: string): string;
var
  LError: TError;
begin
  Result := '';
  for LError in FValidator.Validate(Self, Name) do
    Result := Result + LError.ErrorMessage;
end;

function TAddGameViewModel.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TAddGameViewModel.SetTitle(const Value: string);
begin
  FTitle := Value;
  NotifyOfPropertyChange('Title');
  NotifyOfPropertyChange('CanAddGame');
end;

procedure TAddGameViewModel.SetNotes(const Value: string);
begin
  FNotes := Value;
  NotifyOfPropertyChange('Notes');
end;

procedure TAddGameViewModel.SetRating(const Value: double);
begin
  FRating := Value;
  NotifyOfPropertyChange('Rating');
end;

initialization

TAddGameViewModel.ClassName;

end.
