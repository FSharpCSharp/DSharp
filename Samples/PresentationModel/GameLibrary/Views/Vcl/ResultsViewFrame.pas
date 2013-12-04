unit ResultsViewFrame;

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
  TResultsView = class(TFrame)
    StatusMessage: TGroupBox;
    Results: TScrollBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization

TResultsView.ClassName;

end.
