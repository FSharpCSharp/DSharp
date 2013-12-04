program TasksFmx;

uses
{$ifdef CodeSite}
  DSharp.Logging.CodeSite,
{$endif CodeSite}
  DSharp.PresentationModel.FMXApplication,
  FMX.Forms,
  Interfaces in 'Interfaces.pas',
  ShellViewModel in 'ViewModels\ShellViewModel.pas',
  TasksViewModel in 'ViewModels\TasksViewModel.pas',
  TaskViewModel in 'ViewModels\TaskViewModel.pas',
  ShellViewForm in 'Views\Fmx\ShellViewForm.pas' {ShellView},
  TasksViewForm in 'Views\Fmx\TasksViewForm.pas' {TasksView},
  TaskViewEditForm in 'Views\Fmx\TaskViewEditForm.pas' {TaskViewEdit},
  TaskViewForm in 'Views\Fmx\TaskViewForm.pas' {TaskView},
  TaskViewNewForm in 'Views\Fmx\TaskViewNewForm.pas' {TaskViewNew},
  TaskViewViewForm in 'Views\Fmx\TaskViewViewForm.pas' {TaskViewView};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
{$ifdef DEBUG}
  Application.WithDebugLogger();
{$endif DEBUG}
{$ifdef CodeSite}
  Application.WithLogger<TCodeSiteLog>();
{$endif CodeSite}
  Application.Start<IShellViewModel>();
end.
