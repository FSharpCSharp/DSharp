unit MyDialogViewFrame;

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
  ActnList,
  Actions,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TMyDialogView = class(TFrame)
    OK: TButton;
    Value: TEdit;
    Cancel: TButton;
    ActionList1: TActionList;
    ActionOK: TAction;
    ActionCancel: TAction;
    DisplayName: TLabel;
  end;

implementation

{$R *.dfm}

initialization

TMyDialogView.ClassName;

end.
