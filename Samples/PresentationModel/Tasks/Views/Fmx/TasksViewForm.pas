unit TasksViewForm;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Controls,
  FMX.Edit,
  FMX.Forms,
  FMX.Layouts,
  FMX.Types,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  /// <summary>
  /// View for ShellView
  /// </summary>
  TTasksView = class(TForm)
    NewTaskItem: TLayout;
    Panel2: TLayout;
    ItemsCount: TLabel;
    Tasks: TScrollBox;
    All: TButton;
    Completed: TButton;
    Container: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.FMX}

initialization

TTasksView.ClassName;

end.
