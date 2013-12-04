unit zFrameB;

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
  Commands,
  DSharp.PresentationModel.EventAggregatorIntf,
  ExtCtrls;

type
  TFrameB = class(TFrame, IHandle<TLogMessage>, IHandle<TCommandB>)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  strict protected
    procedure Log(const Line: string); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Handle(AMessage: TLogMessage); overload;
    procedure Handle(AMessage: TCommandB); overload;
  end;

implementation

{$R *.dfm}

constructor TFrameB.Create(AOwner: TComponent);
begin
  inherited;
  EventAggregator.Subscribe(Self);
end;

procedure TFrameB.Button1Click(Sender: TObject);
begin
  EventAggregator.PublishOnUIThread(TCommandA.Create('Command for A'));
end;

procedure TFrameB.Handle(AMessage: TLogMessage);
begin
  Log(AMessage.Message);
end;

procedure TFrameB.Handle(AMessage: TCommandB);
begin
  Log(AMessage.ToString);
end;

procedure TFrameB.Log(const Line: string);
begin
  Memo1.Lines.Add(Line);
  Memo1.Lines.Add('');
end;

end.
