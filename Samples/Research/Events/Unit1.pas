unit Unit1;

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
  DSharp.Core.Events, Vcl.ExtCtrls;

type
  {$M+}
  TSomeEvent = reference to procedure(Sender: TObject; Args: string);

  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    ButtonAdd_Hi: TButton;
    ButtonInvoke: TButton;
    ButtonAdd_Anonymous: TButton;
    ButtonAdd_ClassHi: TButton;
    Button5: TButton;
    ButtonClear: TButton;
    procedure ButtonAdd_HiClick(Sender: TObject);
    procedure ButtonInvokeClick(Sender: TObject);
    procedure ButtonAdd_AnonymousClick(Sender: TObject);
    procedure ButtonAdd_ClassHiClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  strict protected
    procedure Log(const Line: string); overload;
    procedure Log(const Description: string; const Sender: TObject; const Arg: string); overload;
  strict private
    FHandler: Event<TSomeEvent>;
    class procedure ClassHi(Sender: TObject; Args: string);
    function GetHandler: IEvent<TSomeEvent>;
    procedure Hi(Sender: TObject; Args: string);
    procedure OnViewLoaded(View: TObject);
  public
    property Handler: IEvent<TSomeEvent> read GetHandler;
  end;

  View = class
    class procedure ExecuteOnLoad(Sender: TObject; Handler: TSomeEvent);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonAdd_HiClick(Sender: TObject);
begin
  Handler.Add(Hi);
end;

procedure TForm1.ButtonInvokeClick(Sender: TObject);
begin
  Handler.Invoke(Self, 'my args');
  Log('');
end;

procedure TForm1.ButtonAdd_AnonymousClick(Sender: TObject);
begin
  Handler.Add(
    procedure(Sender: TObject; Args: string)
    begin
      Log('Hi anonymous', Sender, Args);
    end
  );
end;

procedure TForm1.ButtonAdd_ClassHiClick(Sender: TObject);
begin
  Handler.Add(ClassHi);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  View.ExecuteOnLoad(Self,
    procedure(s: TObject; a: string)
    begin
      OnViewLoaded(s);
    end
  );
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  Handler.Clear();
end;

class procedure TForm1.ClassHi(Sender: TObject; Args: string);
begin
  (Sender as TForm1).Log('ClassHi', Sender, Args);
end;

function TForm1.GetHandler: IEvent<TSomeEvent>;
begin
  Result := FHandler;
end;

procedure TForm1.Hi(Sender: TObject; Args: string);
begin
  Log('Hi', Sender, Args);
end;

procedure TForm1.Log(const Line: string);
begin
  Memo1.Lines.Add(Line);
end;

procedure TForm1.Log(const Description: string; const Sender: TObject; const Arg: string);
begin
  Log(Format('%s; Sender "%s" and Arg "%s"', [Description, Sender.ToString(), Arg]));
end;

procedure TForm1.OnViewLoaded(View: TObject);
begin
  (View as TForm1).Log('OnViewLoaded ' + View.ToString);
end;

{ View }

class procedure View.ExecuteOnLoad(Sender: TObject; Handler: TSomeEvent);
begin
  Handler(Sender, 'my args');
end;

end.
