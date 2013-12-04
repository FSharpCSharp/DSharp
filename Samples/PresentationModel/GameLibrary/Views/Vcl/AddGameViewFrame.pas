unit AddGameViewFrame;

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
  TAddGameView = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Rating: TLabel;
    Label3: TLabel;
    Notes: TMemo;
    AddGame: TButton;
    Title: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

TAddGameView.ClassName;

end.
