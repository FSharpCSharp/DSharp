program GameLibrary;

uses
  DSharp.PresentationModel.VCLApplication,
  Forms,
  ShellViewModel in 'ViewModels\ShellViewModel.pas',
  Interfaces in 'Framework\Interfaces.pas',
  SearchViewModel in 'ViewModels\SearchViewModel.pas',
  ResultsViewModel in 'ViewModels\ResultsViewModel.pas',
  NoResultsViewModel in 'ViewModels\NoResultsViewModel.pas',
  IndividualResultViewModel in 'ViewModels\IndividualResultViewModel.pas',
  AddGameViewModel in 'ViewModels\AddGameViewModel.pas',
  ExploreGameViewModel in 'ViewModels\ExploreGameViewModel.pas',
  GameDTO in 'Model\GameDTO.pas',
  SearchResult in 'Model\SearchResult.pas',
  FakeBackend in 'Model\FakeBackend.pas',
  Show in 'Framework\Show.pas',
  BusyResult in 'Framework\BusyResult.pas',
  OpenResultBase in 'Framework\OpenResultBase.pas',
  OpenResultIntf in 'Framework\OpenResultIntf.pas',
  OpenChildResult in 'Framework\OpenChildResult.pas',
  AddGameViewFrame in 'Views\Vcl\AddGameViewFrame.pas' {AddGameView: TFrame} ,
  ExploreGameViewFrame
    in 'Views\Vcl\ExploreGameViewFrame.pas' {ExploreGameView: TFrame} ,
  GameDTOViewFrame in 'Views\Vcl\GameDTOViewFrame.pas' {GameDTOView: TFrame} ,
  IndividualResultViewFrame
    in 'Views\Vcl\IndividualResultViewFrame.pas' {IndividualResultView: TFrame} ,
  NoResultsViewFrame
    in 'Views\Vcl\NoResultsViewFrame.pas' {NoResultsView: TFrame} ,
  ResultsViewFrame in 'Views\Vcl\ResultsViewFrame.pas' {ResultsView: TFrame} ,
  SearchViewFrame in 'Views\Vcl\SearchViewFrame.pas' {SearchView: TFrame} ,
  ShellViewForm in 'Views\Vcl\ShellViewForm.pas' {ShellView} ,
  ValidatorIntf in 'Framework\ValidatorIntf.pas',
  Error in 'Framework\Error.pas',
  Validator in 'Framework\Validator.pas',
  Themes;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.WithDebugLogger();
  Application.Start<IShellViewModel>();

end.
