unit zFrameFirst;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TFrameFirst = class(TFrame)
    FirstHeader: TPanel;
    FirstFooter: TPanel;
    FirstContent: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  zFrameSecond,
  zShared;

{$R *.dfm}

constructor TFrameFirst.Create(AOwner: TComponent);
begin
  inherited;
  SetContentPropertyCore(FirstContent, TFrameSecond.Create(Manager));
end;

destructor TFrameFirst.Destroy;
begin
  Logger.LogInfo('Destroying ' + Self.Name);
  inherited;
end;

end.
