unit ShellViewModel;

interface

uses
  SysUtils,
  Interfaces,
  DSharp.Collections,
  DSharp.PresentationModel,
  TasksViewModel;

type

  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  // [Export(TypeInfo(IShellViewModel))]
  [PartCreationPolicy(TCreationPolicy.cpShared)]
  TShellViewModel = class(TScreen, IShellViewModel)
  strict private
    FTasks: ITasksViewModel;
  public
    constructor Create(Tasks: ITasksViewModel);
    property Tasks: ITasksViewModel read FTasks;
  end;

implementation

constructor TShellViewModel.Create(Tasks: ITasksViewModel);
begin
  inherited Create;
  FTasks := Tasks;
end;

initialization

TShellViewModel.ClassName;

end.
