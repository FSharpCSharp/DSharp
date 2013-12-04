unit TaskViewNewFrame;

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
  TTaskViewNew = class(TFrame)
    Add: TButton;
    Bevel1: TBevel;
    [Binding('OnKeyPress', '{Binding DescriptionKeyPress}')]
    Description: TEdit;
  end;

implementation

{$R *.dfm}

initialization

TTaskViewNew.ClassName;

end.
