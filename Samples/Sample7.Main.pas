unit Sample7.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sample7.Customer, Collections.Base, Collections.Lists, StdCtrls;
  // You need http://code.google.com/p/delphi-coll/ for this sample to compile

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    customers: TObjectList<TCustomer>;
    filter1: TFunc<TCustomer, Boolean>;
    filter2: TFunc<TCustomer, Boolean>;
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
  Rtti,
  StrUtils;

type
  TStartsTextExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

function TStartsTextExpression.Compile: TValue;
begin
  Result := TValue.From<Boolean>(StrUtils.StartsText(Left.Compile.ToString, Right.Compile.ToString));
end;

function TStartsTextExpression.ToString: string;
begin
  Result := Format('StartsText(%s, %s)', [Left.ToString(), Right.ToString()]);
end;

function StartsText(SubText, Text: Variant): IExpression;
var
  LLeft, LRight: IExpression;
begin
  LRight := ExpressionStack.Pop();
  LLeft := ExpressionStack.Pop();
  Result := TStartsTextExpression.Create(LLeft, LRight);
  ExpressionStack.Push(Result);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  cust: TCustomer;
begin
  Memo1.Lines.Add(TLambda.GetExpression(filter1).ToString);
  for cust in customers.Where(filter1) do
  begin
    Memo1.Lines.Add(Format('%s %s', [cust.CustomerId, cust.CompanyName]));
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  cust: TCustomer;
begin
  Memo1.Lines.Add(TLambda.GetExpression(filter2).ToString);
  for cust in customers.Where(filter2) do
  begin
    Memo1.Lines.Add(Format('%s %s', [cust.CustomerId, cust.CompanyName]));
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  filter1 := TLambda.Make<TCustomer, Boolean>(
    Bool(Arg1.CompanyName = 'Alfreds Futterkiste')
      or Bool(Arg1.CompanyName = 'Around the Horn'));
  filter2 := TLambda.Make<TCustomer, Boolean>(StartsText(Arg(Edit1).Text, Arg1.CompanyName));

  customers := TObjectList<TCustomer>.Create();
  customers.OwnsObjects := True;
  customers.Add(TCustomer.Create('ALFKI', 'Alfreds Futterkiste'));
  customers.Add(TCustomer.Create('ANATR', 'Ana Trujillo Emparedados y helados'));
  customers.Add(TCustomer.Create('ANTON', 'Antonio Moreno Taquería'));
  customers.Add(TCustomer.Create('AROUT', 'Around the Horn'));
  customers.Add(TCustomer.Create('BERGS', 'Berglunds snabbköp'));
  customers.Add(TCustomer.Create('BLAUS', 'Blauer See Delikatessen'));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  customers.Free();
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
