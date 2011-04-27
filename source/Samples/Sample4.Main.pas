unit Sample4.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, System.Events, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Button1.OnClick := TEventHandler<TNotifyEvent>.Create<TProc<TObject>>(Self, [
    procedure(Sender: TObject)
    begin
      Memo1.Lines.Add(Sender.ClassName + ' clicked');
    end])
    .Invoke;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
