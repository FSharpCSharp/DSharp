unit zAccountViewDetails;

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
  TAccountViewDetails = class(TFrame)
    Label1: TLabel;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  AccountViewDetails: TAccountViewDetails;

implementation

{$R *.dfm}
{ TAccountViewDetails }

constructor TAccountViewDetails.Create(AOwner: TComponent);
begin
  inherited;
  Label1.Caption := 'Details ' + sLineBreak +
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

initialization

TAccountViewDetails.ClassName;

end.
