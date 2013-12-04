unit zAccountView;

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
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls, System.Actions;

type
  TAccountView = class(TFrame)
    ActionAuthorizations: TAction;
    ActionDetails: TAction;
    ActionGroups: TAction;
    ActionList1: TActionList;
    ActiveState: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    AccountName: TLabel;
  end;

var
  AccountView: TAccountView;

implementation

{$R *.dfm}

initialization

TAccountView.ClassName;

end.
