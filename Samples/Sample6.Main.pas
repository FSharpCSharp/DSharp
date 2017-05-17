unit Sample6.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    func: TFunc<Integer, Integer>;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DSharp.Core.Expressions,
  DSharp.Core.Lambda,
  Rtti;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(func(42)));
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  e: IExpression;
begin
  e := Lambda.GetExpression(func);
  Memo1.Lines.Add(Format('%s Result: %s',
    [e.ToString, e.Execute().ToString]));
  // setting arg for result to show
  ExpressionParams[0].Value := TValue.From<Integer>(12);
  Memo1.Lines.Add(Format('%s Result: %s',
    [e.ToString, e.Execute().ToString]));
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  e: IExpression;
begin
  e := TMultiplicationExpression.Create(
    TConstantExpression.Create(12),
    TConstantExpression.Create(5));
  Lambda.SetExpression(func, e);
  Memo1.Lines.Add(e.ToString);
  Memo1.Lines.Add(IntToStr(func(0)));
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  ShowMessage(Expression.PropertyAccess(Expression.Constant(Self), 'Caption').Value.ToString);
  Expression.MethodCall(Expression.Constant(Self), 'Button2Click', [Expression.Empty]).Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  func := Lambda.Make<Integer, Integer>(Arg1 * Arg(32));
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
