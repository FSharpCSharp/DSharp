unit zIdentitiesView;

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
  DSharp.Bindings.VCLControls, System.Actions;

type
  TIdentitiesView = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    ActionList1: TActionList;
    ActionShowSelectedAccount: TAction;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  IdentitiesView: TIdentitiesView;

implementation

{$R *.dfm}
{ TIdentitiesView }

constructor TIdentitiesView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Label2.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now);
end;

initialization

TIdentitiesView.ClassName;

end.
