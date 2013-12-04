unit zUnit1;

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
  ExtCtrls,
  ActnList,
  StdCtrls,
  DSharp.PresentationModel.EventAggregatorIntf,
  Commands,
  Rtti,
  System.Actions;

type
  TForm1 = class(TForm, IHandle<ICustomerCreated>, IHandle<ICustomerDeleted>,
    IHandle<TObject>, IHandle<ICustomerMessage>)
    PanelClient: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    Memo1: TMemo;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict protected
    procedure Log(AMessage: string);
  public
    // Handles only ICustomerCreated
    procedure Handle(AMessage: ICustomerCreated); overload;
    // Handles only ICustomerDeleted
    procedure Handle(AMessage: ICustomerDeleted); overload;
    // Will handle every message in the application
    procedure Handle(AMessage: TObject); overload;
    // Handles ICustomerCreated and ICustomerDeleted
    procedure Handle(AMessage: ICustomerMessage); overload;
  end;

var
  Form1: TForm1;

implementation

uses
  TypInfo,
  zFrameA,
  zFrameB,
  DSharp.Core.Reflection;

{$R *.dfm}

procedure TForm1.Action1Execute(Sender: TObject);
begin
  // This will send TCustomerMessage to all assignable handlers
  EventAggregator.PublishOnUIThread(TCustomerMessage.Create('Hello message!',
    'Created @', 'Deleted @'));
end;

procedure TForm1.Action2Execute(Sender: TObject);
begin
  EventAggregator.PublishOnUIThread(TLogMessage.Create(DateTimeToStr(Now)));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EventAggregator.Subscribe(Self);
  TFrameA.Create(Self).Parent := PanelClient;
  TFrameB.Create(Self).Parent := PanelClient;
end;

procedure TForm1.Handle(AMessage: ICustomerCreated);
begin
  Log('ICustomerCreated handler: ' + AMessage.Message);
end;

procedure TForm1.Handle(AMessage: ICustomerDeleted);
begin
  Log('ICustomerDeleted handler: ' + AMessage.Message);
end;

procedure TForm1.Handle(AMessage: TObject);
begin
  Log('TObject handler: ' + AMessage.ToString);
end;

procedure TForm1.Handle(AMessage: ICustomerMessage);
begin
  Log('ICustomerMessage handler: ' + AMessage.Message);
end;

procedure TForm1.Log(AMessage: string);
begin
  Memo1.Lines.Add(AMessage);
  Memo1.Lines.Add('');
end;

end.
