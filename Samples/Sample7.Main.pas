unit Sample7.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sample7.Customer, DSharp.Collections,
  DSharp.Collections.Extensions;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    customers: IList<TCustomer>;
    filter1: TPredicate<TCustomer>;
    filter2: TPredicate<TCustomer>;
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
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

function TStartsTextExpression.CompileExpression: TCompiledExpression;
var
  LeftDelegate, RightDelegate: TFunc<TValue>;
begin
  LeftDelegate := Left.Compile();
  RightDelegate := Right.Compile();

  Result :=
    function: TValue
    begin
      Result := TValue.From<Boolean>(StrUtils.StartsText(LeftDelegate().ToString, RightDelegate().ToString));
    end;
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
  Memo1.Lines.Add(Lambda.GetExpression(filter1).ToString);
  for cust in Enumerable<TCustomer>(customers).Where(filter1) do
  begin
    Memo1.Lines.Add(Format('%s %s', [cust.CustomerId, cust.CompanyName]));
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  cust: TCustomer;
begin
  Memo1.Lines.Add(Lambda.GetExpression(filter2).ToString);
  for cust in Enumerable<TCustomer>(customers).Where(filter2) do
  begin
    Memo1.Lines.Add(Format('%s %s', [cust.CustomerId, cust.CompanyName]));
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  filter1 := Lambda.Predicate<TCustomer>(
    Bool(Arg1.CompanyName = 'Alfreds Futterkiste')
      or Bool(Arg1.CompanyName = 'Around the Horn'));
  filter2 := Lambda.Predicate<TCustomer>(StartsText(Arg(Edit1).Text, Arg1.CompanyName));

  customers := TObjectList<TCustomer>.Create(True);
  customers.Add(TCustomer.Create('ALFKI', 'Alfreds Futterkiste'));
  customers.Add(TCustomer.Create('ANATR', 'Ana Trujillo Emparedados y helados'));
  customers.Add(TCustomer.Create('ANTON', 'Antonio Moreno Taquería'));
  customers.Add(TCustomer.Create('AROUT', 'Around the Horn'));
  customers.Add(TCustomer.Create('BERGS', 'Berglunds snabbköp'));
  customers.Add(TCustomer.Create('BLAUS', 'Blauer See Delikatessen'));
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
