unit zFormMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    ActivateFirst: TButton;
    Activeitem: TLayout;
    FreeFirst: TButton;
    Layout1: TLayout;
    MemoLog: TMemo;
    procedure ActivateFirstClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FreeFirstClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(Sender: TObject; Value: string);
  end;

var
  Form1: TForm1;

implementation

uses
  zShared,
  zFormFirst;

procedure TForm1.ActivateFirstClick(Sender: TObject);
begin
  SetContentPropertyCore(Activeitem, TFormFirst.Create(Manager));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TLogger(Logger).OnLog := OnLog;
end;

procedure TForm1.FreeFirstClick(Sender: TObject);
begin
  SetContentPropertyCore(Activeitem, nil);
end;

procedure TForm1.OnLog(Sender: TObject; Value: string);
begin
  if not(csDestroying in ComponentState) then
    MemoLog.Lines.Add(Value);
end;

{$R *.fmx}

end.
