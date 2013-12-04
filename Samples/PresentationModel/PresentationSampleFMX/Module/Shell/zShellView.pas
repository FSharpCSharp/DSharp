unit zShellView;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.StdCtrls,
  DSharp.Bindings.FMXControls;

type
  TShellView = class(TForm)
    Panel1: TPanel;
    ActiveItem: TLayout;
    ActionShowDocuments: TButton;
    ActionShowIdentities: TButton;
    HelloButton: TButton;
    LabelInfo: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TShellView.ClassName;

end.
