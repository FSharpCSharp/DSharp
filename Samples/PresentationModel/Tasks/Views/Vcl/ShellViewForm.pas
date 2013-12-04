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
  DSharp.Bindings.VCLControls;

type
  ///	<summary>
  ///	  View for ShellView
  ///	</summary>
  TShellView = class(TForm)
    GridPanel1: TGridPanel;
    Tasks: TPanel;
    Label1: TLabel;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
