unit TasksViewFrame;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  View for ShellView
  ///	</summary>
  TTasksView = class(TFrame)
    NewTaskItem: TPanel;
    PanelBottom: TPanel;
    ItemsCount: TLabel;
    Tasks: TScrollBox;
    All: TButton;
    Completed: TButton;
  end;

implementation

{$R *.dfm}

initialization

TTasksView.ClassName;

end.
