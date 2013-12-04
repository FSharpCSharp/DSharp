unit TaskViewEditForm;

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
  /// View for TaskView
  /// </summary>
  TTaskViewEdit = class(TForm)
    IsCompleted: TCheckBox;
    Remove: TButton;
    Bevel1: TLayout;
    [Binding('OnKeyPress', '{Binding DescriptionKeyPress}')]
    [Binding('OnExit', '{Binding DescriptionExit}')]
    Description: TEdit;
    Container: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.FMX}

initialization

TTaskViewEdit.ClassName;

end.
