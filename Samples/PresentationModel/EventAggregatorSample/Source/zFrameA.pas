unit zFrameA;

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
  TFrameA = class(TFrame, IHandle<TLogMessage>, IHandle<TCommandA>)
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
    procedure Handle(AMessage: TCommandA); overload;
  end;

implementation

{$R *.dfm}
{ TFrameA }

constructor TFrameA.Create(AOwner: TComponent);
begin
  inherited;
  EventAggregator.Subscribe(Self);
end;

procedure TFrameA.Button1Click(Sender: TObject);
begin
  EventAggregator.PublishOnUIThread(TCommandB.Create('Command for B'));
end;

procedure TFrameA.Handle(AMessage: TCommandA);
begin
  Log(AMessage.ToString);
end;

procedure TFrameA.Log(const Line: string);
begin
  Memo1.Lines.Add(Line);
end;

procedure TFrameA.Handle(AMessage: TLogMessage);
begin
  Log(AMessage.Message);
  Memo1.Lines.Add('');
end;

end.
