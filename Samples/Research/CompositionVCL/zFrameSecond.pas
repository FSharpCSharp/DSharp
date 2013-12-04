unit zFrameSecond;

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
  Vcl.ExtCtrls;

type
  TFrameSecond = class(TFrame)
    SecondHeader: TPanel;
    SecondFooter: TPanel;
    SecondContent: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
    destructor Destroy; override;
  end;

implementation

uses
  zShared;

{$R *.dfm}

destructor TFrameSecond.Destroy;
begin
  Logger.LogInfo('Destroying ' + Self.Name);
  inherited;
end;

end.
