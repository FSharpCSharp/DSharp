unit ShellViewForm;

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
  ///	  View for ShellView
  ///	</summary>
  TShellView = class(TForm)
    PanelLeftButtons: TPanel;
    ActiveItem: TPanel;
    ShowExampleOne: TButton;
    ShowExampleTwo: TButton;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
