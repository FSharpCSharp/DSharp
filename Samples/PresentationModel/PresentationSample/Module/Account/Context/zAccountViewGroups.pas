unit zAccountViewGroups;

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
  DSharp.Bindings.VCLControls;

type
  TAccountViewGroups = class(TFrame)
    Label1: TLabel;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  AccountViewGroups: TAccountViewGroups;

implementation

{$R *.dfm}
{ TAccountViewGroups }

constructor TAccountViewGroups.Create(AOwner: TComponent);
begin
  inherited;
  Label1.Caption := 'Groups' + sLineBreak +
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

initialization

TAccountViewGroups.ClassName;

end.
