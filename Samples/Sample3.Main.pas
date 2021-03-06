unit Sample3.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  DSharp.Collections.Iterators,
  Rtti,
  Spring.Collections;

procedure Enumerate;
var
  a, b, c: UInt64;
  Result: Yield<UInt64>;
begin
  a := 0;
  b := 1;
  Result := a;
  Result := b;
  repeat
    c := a + b;
    a := b;
    b := c;
    Result := c;
  until c > High(UInt64) div 2;
end;

function Power(ANumber, AExponent: Integer): IEnumerable<Integer>;
begin
  Result := TIteratorBlock<Integer>.Create(
    procedure
    var
      i, k: Integer;
      Result: Yield<Integer>;
    begin
      k := 1;
      for i := 1 to AExponent do
      begin
        k := k * ANumber;
        Result := k;
      end;
    end);
end;

function Fibonacci: IEnumerable<UInt64>;
begin
  Result := TIteratorBlock<UInt64>.Create(Enumerate);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: UInt64;
//  i: TValue;
begin
  Memo1.Clear();
  Memo1.Lines.BeginUpdate();
  try
    for i in Fibonacci() do
    begin
      Memo1.Lines.Add(UIntToStr(i));
//      Memo1.Lines.Add(i.ToString);
    end;
  finally
    Memo1.Lines.EndUpdate();
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  Memo1.Clear();
  Memo1.Lines.BeginUpdate();
  try
    for i in Power(2, 10) do
    begin
      Memo1.Lines.Add(IntToStr(i));
    end;
  finally
    Memo1.Lines.EndUpdate();
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
