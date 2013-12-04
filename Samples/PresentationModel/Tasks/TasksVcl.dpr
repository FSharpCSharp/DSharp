program TasksVcl;

// Remove the comment marks in front of CodeSite to use that as a logging destination.
uses
  Forms,
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Interfaces in 'Interfaces.pas',
  ShellViewModel in 'ViewModels\ShellViewModel.pas',
  ShellViewForm in 'Views\Vcl\ShellViewForm.pas' {ShellView} ,
  TasksViewModel in 'ViewModels\TasksViewModel.pas',
  TaskViewModel in 'ViewModels\TaskViewModel.pas',
  TasksViewFrame in 'Views\Vcl\TasksViewFrame.pas' {TasksView: TFrame} ,
  TaskViewEditFrame
    in 'Views\Vcl\TaskViewEditFrame.pas' {TaskViewEdit: TFrame} ,
  TaskViewFrame in 'Views\Vcl\TaskViewFrame.pas' {TaskView: TFrame} ,
  TaskViewNewFrame in 'Views\Vcl\TaskViewNewFrame.pas' {TaskViewNew: TFrame} ,
  TaskViewViewFrame in 'Views\Vcl\TaskViewViewFrame.pas' {TaskViewView: TFrame};

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  {$IFDEF DEBUG}
  Application.WithDebugLogger();
  {$ENDIF DEBUG}
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>();
  {$ENDIF CodeSite}
  Application.Start<IShellViewModel>();

  // Application.WithDebugLogger.Start<IShellViewModel>;
  // Application.WithLogger<TCodeSiteLog>().Start<IShellViewModel>;
end.
