unit TaskViewFrame;

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
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  View for TaskView
  ///	</summary>
  TTaskView = class(TFrame)
    // The following two lines are not neccessary anymore because binding to context screens is now handled by the framework!
    // [Binding('View.Context', '{Binding Mode}')]
    // [Binding('View.Model', '{Binding}')]
    Mode: TPanel;
  end;

implementation

{$R *.dfm}

initialization

TTaskView.ClassName;

end.
