unit TasksViewModel;

interface

uses
  SysUtils,
  Interfaces,
  DSharp.Collections,
  DSharp.PresentationModel;

type
  {$SCOPEDENUMS ON}
  TTasksFilter = (All, Completed);
  {$SCOPEDENUMS OFF}

  ///	<summary>
  ///	  Implementation of <see cref="TTasksViewModel" />
  ///	</summary>
  // [Export(TypeInfo(TTasksViewModel))]
  [PartCreationPolicy(TCreationPolicy.cpShared)]
  TTasksViewModel = class(TScreen, ITasksViewModel)
  strict private
    FFilter: TTasksFilter;
    FNewTaskItem: IScreen;
    FSelectedTask: ITaskViewModel;
    FTasks: IList<ITaskViewModel>;
    function GetCanFilterAll: Boolean;
    function GetItemsCount: string;
    procedure SetFilter(const Value: TTasksFilter);
    procedure SetSelectedTask(const Value: ITaskViewModel);
    procedure OnTasksCollectionChangedEvent(Sender: TObject;
      const Item: ITaskViewModel; Action: TCollectionChangedAction);
  public
    constructor Create(NewTask: ITaskViewModel);
    procedure All;
    procedure Completed;
    procedure NewTask(Task: ITaskViewModel);
    procedure RemoveTask(Task: ITaskViewModel);
    property CanFilterAll: Boolean read GetCanFilterAll;
    property Filter: TTasksFilter read FFilter write SetFilter;
    property ItemsCount: string read GetItemsCount;
    property NewTaskItem: IScreen read FNewTaskItem;
    property SelectedTask: ITaskViewModel read FSelectedTask
      write SetSelectedTask;
    property Tasks: IList<ITaskViewModel> read FTasks;
  end;

implementation

constructor TTasksViewModel.Create(NewTask: ITaskViewModel);
begin
  inherited Create;
  FTasks := TList<ITaskViewModel>.Create;
  FTasks.OnCollectionChanged.Add(OnTasksCollectionChangedEvent);
  FNewTaskItem := NewTask as IScreen;
  FFilter := TTasksFilter.All;
end;

procedure TTasksViewModel.All;
begin
  Filter := TTasksFilter.All;
end;

procedure TTasksViewModel.Completed;
begin
  Filter := TTasksFilter.Completed;
end;

function TTasksViewModel.GetCanFilterAll: Boolean;
begin
  Result := Tasks.Count > 0;
end;

function TTasksViewModel.GetItemsCount: string;
begin
  Result := Format('%d items', [Tasks.Count]);
end;

procedure TTasksViewModel.NewTask(Task: ITaskViewModel);
begin
  FTasks.Add(Task);
  NotifyOfPropertyChange('Tasks');
  NotifyOfPropertyChange('ItemsCount');
  NotifyOfPropertyChange('CanFilterAll');
end;

procedure TTasksViewModel.OnTasksCollectionChangedEvent(Sender: TObject;
  const Item: ITaskViewModel; Action: TCollectionChangedAction);
begin
end;

procedure TTasksViewModel.RemoveTask(Task: ITaskViewModel);
begin
  // Remove calling button from callstack; works around issues like these: http://qc.embarcadero.com/wc/qcmain.aspx?d=112018
  Execute.BeginOnUIThread(
    procedure
    begin
      Tasks.Remove(Task);
      NotifyOfPropertyChange('Tasks');
      NotifyOfPropertyChange('ItemsCount');
      NotifyOfPropertyChange('CanFilterAll');
    end);
end;

procedure TTasksViewModel.SetFilter(const Value: TTasksFilter);
begin
  FFilter := Value;
  NotifyOfPropertyChange('Filter');
end;

procedure TTasksViewModel.SetSelectedTask(const Value: ITaskViewModel);
begin
  FSelectedTask := Value;
  NotifyOfPropertyChange('SelectedTask');
end;

initialization

TTasksViewModel.ClassName;

end.
