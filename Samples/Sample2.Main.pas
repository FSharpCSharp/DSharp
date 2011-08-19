unit Sample2.Main;

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
    procedure Changed(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  DSharp.Core.Properties;

{$IF COMPILERVERSION < 22}{$Message Warn 'Feature in this sample is not supported in Delphi 2010'}{$IFEND}
type
  TFoo = class
  private
    FCount: TField<Integer>;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: TProperty<Integer> read FCount.Value write FCount.Value;
  end;

{ TFoo }

constructor TFoo.Create;
begin
  FCount.Initialize(Self);
end;

destructor TFoo.Destroy;
begin
  FCount.Finalize();
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  f: TFoo;
begin
  f := TFoo.Create();
  f.Count.OnChange.Add(Changed);
  Memo1.Lines.Add('changing property...');
  f.Count := 12;
  Memo1.Lines.Add('value of property: ' + f.Count.ToString);
  Memo1.Lines.Add('changing property...');
  f.Count := 13;
  Memo1.Lines.Add('value of property: ' + f.Count.ToString);
  f.Free();
end;

procedure TForm1.Changed(Sender: TObject);
begin
  Memo1.Lines.Add('property changed to: ' + (Sender as TFoo).Count.ToString);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
