unit ColorViewFrame;

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
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  View for ColorView
  ///	</summary>
  TColorView = class(TFrame)
    Red: TRadioButton;
    Green: TRadioButton;
    Blue: TRadioButton;
  end;

implementation

{$R *.dfm}

initialization

TColorView.ClassName;

end.
