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
  ShellViewModel,
  StdCtrls,
  ExtCtrls,
  ActnList,
  DSharp.Bindings.VCLControls;

type
  TShellView = class(TForm)
    ActiveItem: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    BusyIndicator: TPanel;
    Image1: TImage;
    Back: TImage;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
