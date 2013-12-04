unit ExploreGameViewFrame;

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
  TExploreGameView = class(TFrame)
    Game: TPanel;
    [Binding('Visible', '{Binding IsCheckedIn}')]
    Panel1: TPanel;
    Label1: TLabel;
    [Binding('Visible', '{Binding IsCheckedOut}')]
    Panel2: TPanel;
    BorrowedMessage: TLabel;
    Borrower: TEdit;
    Panel3: TPanel;
    CheckIn: TButton;
    Panel4: TPanel;
    CheckOut: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

TExploreGameView.ClassName;

end.
