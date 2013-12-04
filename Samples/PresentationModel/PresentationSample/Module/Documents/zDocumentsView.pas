unit zDocumentsView;

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
  TDocumentsView = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  DocumentsView: TDocumentsView;

implementation

{$R *.dfm}
{ TDocumentsView }

constructor TDocumentsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Label2.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now);
end;

initialization

TDocumentsView.ClassName;

end.
