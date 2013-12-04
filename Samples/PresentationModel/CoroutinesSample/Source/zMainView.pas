unit zMainView;

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
  zMainViewModel,
  StdCtrls,
  ExtCtrls,
  ActnList,
  DSharp.Bindings.VCLControls, System.Actions;

type
  TMainView = class(TForm)
    Results: TMemo;
    Panel1: TPanel;
    AddGame: TButton;
    ActionList1: TActionList;
    ShowGame: TAction;
    Button1: TButton;
  end;

implementation

{$R *.dfm}

initialization

TMainView.ClassName;

end.
