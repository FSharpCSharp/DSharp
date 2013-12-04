program GameLibraryFMX;

uses
  DSharp.PresentationModel.FMXApplication,
  FMX.Forms,
  FMX.Types,
  GameDTO in 'Model\GameDTO.pas',
  SearchResult in 'Model\SearchResult.pas',
  FakeBackend in 'Model\FakeBackend.pas',
  AddGameViewModel in 'ViewModels\AddGameViewModel.pas',
  ExploreGameViewModel in 'ViewModels\ExploreGameViewModel.pas',
  IndividualResultViewModel in 'ViewModels\IndividualResultViewModel.pas',
  NoResultsViewModel in 'ViewModels\NoResultsViewModel.pas',
  ResultsViewModel in 'ViewModels\ResultsViewModel.pas',
  SearchViewModel in 'ViewModels\SearchViewModel.pas',
  ShellViewModel in 'ViewModels\ShellViewModel.pas',
  AddGameViewForm in 'Views\Fmx\AddGameViewForm.pas' {AddGameView} ,
  ExploreGameViewForm in 'Views\Fmx\ExploreGameViewForm.pas' {ExploreGameView} ,
  GameDTOViewForm in 'Views\Fmx\GameDTOViewForm.pas' {GameDTOView} ,
  IndividualResultViewForm
    in 'Views\Fmx\IndividualResultViewForm.pas' {IndividualResultView} ,
  NoResultsViewForm in 'Views\Fmx\NoResultsViewForm.pas' {NoResultsView} ,
  ResultsViewForm in 'Views\Fmx\ResultsViewForm.pas' {ResultsView} ,
  SearchViewForm in 'Views\Fmx\SearchViewForm.pas' {SearchView} ,
  ShellViewForm in 'Views\Fmx\ShellViewForm.pas' {ShellView} ,
  BusyResult in 'Framework\BusyResult.pas',
  Error in 'Framework\Error.pas',
  Interfaces in 'Framework\Interfaces.pas',
  OpenChildResult in 'Framework\OpenChildResult.pas',
  OpenResultBase in 'Framework\OpenResultBase.pas',
  OpenResultIntf in 'Framework\OpenResultIntf.pas',
  Show in 'Framework\Show.pas',
  Validator in 'Framework\Validator.pas',
  ValidatorIntf in 'Framework\ValidatorIntf.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.WithDebugLogger();
  Application.Start<IShellViewModel>();

end.
