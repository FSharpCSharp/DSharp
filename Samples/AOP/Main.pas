unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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
  DSharp.Core.Aspects,
  DSharp.Core.Aspects.Logging,
// uncomment the following line if you dont have SmartInspect - use Console logging instead
// support for CodeSite coming soon
  DSharp.Core.Logging.SmartInspect;
//  DSharp.Core.Logging.Console;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // following code adds the logging aspect to every virtual method that is
  // accessable by RTTI (public and published by default)

  // using the $RTTI compiler directive in your own units can make
  // private and protected methods accessable so aspects can be weaved into

//  AspectWeaver.AddAspect(TForm1, TLoggingAspect, '.*');
//  AspectWeaver.AddAspect(TForm1, TLoggingAspect, '^((?!InitiateAction).)*$');
  AspectWeaver.AddAspect(TButton, TLoggingAspect, '.*');
  AspectWeaver.AddAspect(TEdit, TLoggingAspect, '.*');
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
