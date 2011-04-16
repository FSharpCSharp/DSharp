unit Sample3.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{.$DEFINE USE_COLLECTIONS}

uses
  Collections.Yield,
  System.Fibers,
{$IFDEF USE_COLLECTIONS}
  Collections.Base;        
{$ELSE}
  Generics.Collections;
{$ENDIF}

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

{$IFDEF USE_COLLECTIONS}
function Fibonacci: IEnumerable<UInt64>;
{$ELSE}
function Fibonacci: TEnumerable<UInt64>;
{$ENDIF}
begin
  Result := TDelegateEnumerable<UInt64>.Create(Enumerate);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
{$IFDEF USE_COLLECTIONS}
  numbers: IEnumerable<UInt64>;
{$ELSE}
  numbers: TEnumerable<UInt64>;
{$ENDIF}
  i: UInt64;
begin
  numbers := Fibonacci();
  Memo1.Clear();
  Memo1.Lines.BeginUpdate();
  for i in numbers do
  begin
    Memo1.Lines.Add(UIntToStr(i));
  end;                   
  Memo1.Lines.EndUpdate();
{$IFNDEF USE_COLLECTIONS}
  numbers.Free();
{$ENDIF}
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
