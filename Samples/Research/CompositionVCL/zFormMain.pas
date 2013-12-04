unit zFormMain;

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
  TFormMain = class(TForm)
    ActiveItem: TPanel;
    Header: TPanel;
    ActivateFirst: TButton;
    FreeFirst: TButton;
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ActivateFirstClick(Sender: TObject);
    procedure FreeFirstClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(Sender: TObject; Value: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  zFrameFirst,
  zShared;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TLogger(Logger).OnLog := OnLog;
end;

procedure TFormMain.ActivateFirstClick(Sender: TObject);
begin
  SetContentPropertyCore(ActiveItem, TFrameFirst.Create(Manager));
end;

procedure TFormMain.FreeFirstClick(Sender: TObject);
begin
  SetContentPropertyCore(ActiveItem, nil);
end;

procedure TFormMain.OnLog(Sender: TObject; Value: string);
begin
  if not(csDestroying in ComponentState) then
    MemoLog.Lines.Add(Value);
end;

end.
