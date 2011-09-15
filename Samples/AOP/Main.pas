unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSharp.Core.Aspects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  DSharp.Core.Aspects.Logging,
  DSharp.Core.Logging.SmartInspect;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  AspectWeaver.AddAspect(TForm1, TLoggingAspect, '.*');
//  AspectWeaver.AddAspect(TForm1, TLoggingAspect, '^((?!InitiateAction).)*$');
  AspectWeaver.AddAspect(TButton, TLoggingAspect, '.*');
  AspectWeaver.AddAspect(TEdit, TLoggingAspect, '.*');
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
