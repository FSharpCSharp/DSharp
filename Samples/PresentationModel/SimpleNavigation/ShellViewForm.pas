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
    Panel1: TPanel;
    ActiveItem: TPanel;
    ShowPageOne: TButton;
    ShowPageTwo: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
