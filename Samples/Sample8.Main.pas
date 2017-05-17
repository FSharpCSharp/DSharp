unit Sample8.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowMessage(const Msg: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DSharp.Core.Expressions,
  DSharp.Core.Lambda;

{$IF COMPILERVERSION < 22}{$Message Warn 'Feature in this sample is not supported in Delphi 2010'}{$IFEND}

procedure TMainForm.Button1Click(Sender: TObject);
var
  e: IExpression;
  Result: IValueExpression;
  func: TFunc<Boolean>;
  i: Integer;
begin
  e := Expression.MethodCall(
    Expression.Constant(Self),
    'ShowMessage',
    [Expression.Constant('Hello world')]);
  func := Lambda.Make<Boolean>(e);
  func();

  Result := Expression.Parameter('Result');
  e := Expression.Block([
    Expression.Assign(Result, Expression.Constant(1)),
    Expression.Loop(
      Expression.IfThenElse(
        Expression.GreaterThan(ExpressionParams[0], Expression.Constant(0)),
        Expression.Block([
          Expression.MultiplyAssign(Result, ExpressionParams[0]),
          Expression.SubtractAssign(ExpressionParams[0], Expression.Constant(1))
        ]),
        Expression.Break
      )
    ),
    Result
  ]);

  i := Lambda.Make<Integer, Integer>(e)(5);
  ShowMessage(IntToStr(i));

  Memo1.Lines.Add(e.ToString);
end;

procedure TMainForm.ShowMessage(const Msg: string);
begin
  Dialogs.ShowMessage(Msg);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
