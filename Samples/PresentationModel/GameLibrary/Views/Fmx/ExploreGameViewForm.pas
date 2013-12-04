unit ExploreGameViewForm;

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
  FMX.Objects,
  FMX.Memo,
  FMX.StdCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  TExploreGameView = class(TForm)
    Layout1: TLayout;
    Game: TLayout;
    [Binding('Visible', '{Binding IsCheckedOut}')]
    Rectangle1: TRectangle;
    BorrowedMessage: TLabel;
    CheckIn: TButton;
    [Binding('Visible', '{Binding IsCheckedIn}')]
    Rectangle2: TRectangle;
    Label1: TLabel;
    Borrower: TEdit;
    CheckOut: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

initialization

TExploreGameView.ClassName;

end.
