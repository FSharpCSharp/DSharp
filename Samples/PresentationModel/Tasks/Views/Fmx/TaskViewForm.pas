unit TaskViewForm;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Controls,
  FMX.Edit,
  FMX.Forms,
  FMX.Layouts,
  FMX.Types,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  /// <summary>
  /// View for TaskView
  /// </summary>
  TTaskView = class(TForm)
    // The following two lines are not neccessary anymore because binding to context screens is now handled by the framework!
    // [Binding('View.Context', '{Binding Mode}')]
    // [Binding('View.Model', '{Binding}')]
    Container: TLayout;
    Mode: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.FMX}

initialization

TTaskView.ClassName;

end.
