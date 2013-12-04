unit AddGameViewForm;

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
  FMX.Edit,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  TAddGameView = class(TForm)
    Label1: TLabel;
    Title: TEdit;
    Label2: TLabel;
    Rating: TLabel;
    Label3: TLabel;
    Notes: TMemo;
    AddGame: TButton;
    Layout1: TLayout;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TAddGameView.ClassName;

end.
