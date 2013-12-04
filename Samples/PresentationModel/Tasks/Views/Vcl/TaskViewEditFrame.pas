unit TaskViewEditFrame;

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
  TTaskViewEdit = class(TFrame)
    IsCompleted: TCheckBox;
    Remove: TButton;
    Bevel1: TBevel;
    [Binding('OnKeyPress', '{Binding DescriptionKeyPress}')]
    [Binding('OnExit', '{Binding DescriptionExit}')]
    Description: TEdit;
  end;

implementation

{$R *.dfm}

initialization

TTaskViewEdit.ClassName;

end.
