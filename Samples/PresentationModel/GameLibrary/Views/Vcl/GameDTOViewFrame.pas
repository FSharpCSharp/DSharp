unit GameDTOViewFrame;

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
  TGameDTOView = class(TFrame)
    Label2: TLabel;
    Rating: TLabel;
    Label3: TLabel;
    Notes: TMemo;
    Panel1: TPanel;
    Title: TLabel;
    Panel2: TPanel;
    AddedOn: TLabel;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

TGameDTOView.ClassName;

end.
