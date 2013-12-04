unit zAccountViewAuthorizations;

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
  TAccountViewAuthorizations = class(TFrame)
    Label1: TLabel;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  AccountViewAuthorizations: TAccountViewAuthorizations;

implementation

{$R *.dfm}
{ TAccountViewAuthorizations }

constructor TAccountViewAuthorizations.Create(AOwner: TComponent);
begin
  inherited;
  Label1.Caption := 'Authorizations ' + sLineBreak +
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
end;

initialization

TAccountViewAuthorizations.ClassName;

end.
