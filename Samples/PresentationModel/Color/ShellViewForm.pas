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
    ColorModel: TPanel;
    [Binding('Brush.Color', '{Binding Color}')]
    Shape1: TShape;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
