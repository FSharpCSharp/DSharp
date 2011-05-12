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
  System.Lambda,
  System.Expressions;

procedure TMainForm.Button1Click(Sender: TObject);
var
  e: IExpression;
  v: IParameter;
  func: TFunc<Boolean>;
  i: Integer;
begin
  e := TMethodExpression.Create(
    TValueConstantExpression.Create(Self),
    'ShowMessage',
    [TStringConstantExpression.Create('Hello world')]);
  func := TLambda.Make<Boolean>(e);
  func();

  v := TParameterExpression.Create('Result');
  e := TBlockExpression.Create([
    TAssignExpression.Create(v, TValueConstantExpression.Create(1)),
    TRepeatUntilLoopExpression.Create(
      TIfThenExpression.Create(
        TGreaterThanExpression.Create(ParameterList[0], TValueConstantExpression.Create(0)),
        TBlockExpression.Create([
          TAssignExpression.Create(v,
            TMultiplicationExpression.Create(v, ParameterList[0])
          ),
          TAssignExpression.Create(ParameterList[0],
            TSubtractionExpression.Create(ParameterList[0],
              TValueConstantExpression.Create(1)
            )
          )
        ])
      ),
      TEqualExpression.Create(ParameterList[0], TValueConstantExpression.Create(0))
    )
  ]);

  i := TLambda.Make<Integer, Integer>(e)(5);
  ShowMessage(IntToStr(i));
end;

procedure TMainForm.ShowMessage(const Msg: string);
begin
  Dialogs.ShowMessage(Msg);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
