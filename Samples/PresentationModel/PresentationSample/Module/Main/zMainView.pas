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
  StdCtrls,
  ExtCtrls,
  ActnList,
  DSharp.Bindings.VCLControls;

type
  TMainView = class(TFrame)
    Panel1: TPanel;
    LabelInfo: TLabel;
    ActionClose: TLabel;
    ActionAccount: TLabel;
    ActionSaveAs: TLabel;
    ActionPrint: TLabel;
    ActionShare: TLabel;
    ActionExport: TLabel;
    ActionInfo: TLabel;
    ActionNew: TLabel;
    ActionOpen: TLabel;
    ActionSave: TLabel;
    ActionOptions: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    ActionIdentities: TLabel;
    ActionDocuments: TLabel;
    Panel2: TPanel;
    Header: TPanel;
    ActiveItem: TPanel;
    Label3: TLabel;
    procedure OnItemMouseLeave(Sender: TObject);
    procedure OnItemMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainView: TMainView;

implementation

uses
  zHeaderViewModel;

{$R *.dfm}
{ TMainView }

constructor TMainView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TMainView.OnItemMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Color := $009A572B;
end;

procedure TMainView.OnItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  (Sender as TLabel).Color := $00B56D3E;
end;

initialization

TMainView.ClassName;

end.
