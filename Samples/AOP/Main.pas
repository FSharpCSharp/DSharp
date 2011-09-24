unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
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
  DSharp.Core.Aspects,
  DSharp.Core.Aspects.Logging,
// uncomment one of the following lines to use logging
//  DSharp.Core.Logging.CodeSite;
//  DSharp.Core.Logging.SmartInspect;
//  DSharp.Core.Logging.Console;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit1.Clear;
end;

initialization
  // following code adds the logging aspect to every virtual method that is
  // accessable by RTTI (public and published by default)

  // using the $RTTI compiler directive in your own units can make
  // private and protected methods accessable so aspects can be weaved into

  // the AMethodName is used as regular expression
  // so you can specify which methods to instrument
  AspectWeaver.AddAspect(TWinControl, TLoggingAspect, '^SetFocus$');
  AspectWeaver.AddAspect(TEdit, TLoggingAspect, '^Clear$');
  ReportMemoryLeaksOnShutdown := True;

end.
