unit Sample4.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSharp.Core.Events, StdCtrls;

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
{$IFDEF VER210}
  Button1.OnClick := TEvent<TNotifyEvent>.Create<TProc<TObject>>(Self,
    TArray<TProc<TObject>>.Create(
      procedure(Sender: TObject)
      begin
        Memo1.Lines.Add(Sender.ClassName + ' clicked');
      end))
    .Invoke;
{$ELSE}
  Button1.OnClick := TEvent<TNotifyEvent>.Create<TProc<TObject>>(Self, [
    procedure(Sender: TObject)
    begin
      Memo1.Lines.Add(Sender.ClassName + ' clicked');
    end])
    .Invoke;
{$ENDIF}
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
