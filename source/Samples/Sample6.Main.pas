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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  Rtti,
  System.Expressions,
  System.Lambda;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(func(42)));
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  e: IExpression;
begin
  e := TLambda.GetExpression(func);
  Memo1.Lines.Add(Format('%s Result: %s',
    [e.ToString, e.Compile.ToString]));
  // setting arg for result to show
  ParameterList[0].Value := TValue.From<Integer>(42);
  Memo1.Lines.Add(Format('%s Result: %s',
    [e.ToString, e.Compile.ToString]));
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  e: IExpression;
begin
  e := TMultiplicationExpression.Create(
    TIntegerConstantExpression.Create(12),
    TIntegerConstantExpression.Create(5));
  TLambda.SetExpression(func, e);
  Memo1.Lines.Add(e.ToString);
  Memo1.Lines.Add(IntToStr(func(0)));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  func := TLambda.Make<Integer, Integer>(Arg1 * Arg(32));
end;

end.
