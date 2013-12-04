unit TaskViewViewFrame;

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
  TTaskViewView = class(TFrame)
    IsCompleted: TCheckBox;
    Remove: TButton;
    Bevel1: TBevel;
    [Binding('OnDblClick', '{Binding DescriptionDblClick}')]
    Description: TLabel;
  end;

implementation

{$R *.dfm}

initialization

TTaskViewView.ClassName;

end.
