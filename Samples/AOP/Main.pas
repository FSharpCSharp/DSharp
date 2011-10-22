unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,

  DSharp.Aspects.Logging;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

type
  [Logging]
  IFoo = interface(IInvokable)
    ['{4110E874-18D3-4D38-9698-ED5D98CA9E22}']
    function ShowTextVar(var s: string): Boolean;
    function ShowTextConst(const s: string): Boolean;
    function ShowText(s: string): Boolean;
  end;

//  [Logging]
  TFoo = class(TInterfacedObject, IFoo)
  public
    function ShowTextVar(var s: string): Boolean; virtual;
    function ShowTextConst(const s: string): Boolean; virtual;
    function ShowText(s: string): Boolean; virtual;
  end;

implementation

{$R *.dfm}

uses
// uncomment one of the following lines to enable other logging types
//  DSharp.Logging.Console,
//  DSharp.Logging.SmartInspect,
//  DSharp.Logging.CodeSite,
  DSharp.Logging,
  DSharp.Aspects.Weaver;

procedure TMainForm.Button1Click(Sender: TObject);
var
  foo: IFoo;
  s: string;
begin
  Edit1.Clear;

  // when intercepting classes we just can create the object
  // and it automatically uses all applied aspects
  // even when they are added after the object is created
  // because the class interceptor patches the VMT of the affected classes
  // this restricts the methods that can be intercepted:
  // only public and published virtual methods can be intercepted


  // in this example we will see how interfaces can be intercepted
  // one thing that is different is that the instances need to be proxified
  // individually - a DI container might be a big help in that case
  // how that can be done with spring for example will be shown in
  // some sample in the future

  // you can see the Logging attribute on the IFoo interface
  // that tells the AspectWeaver add that aspect for the IFoo type
  // this behavious is the same to classes and interface with the difference mentioned above

  foo := AspectWeaver.Proxify<IFoo>(TFoo.Create);

  s := 'xyz';
  foo.ShowTextVar(s);
  ShowMessage(s);
end;

//{ TFoo }

function TFoo.ShowText(s: string): Boolean;
begin
  Dialogs.ShowMessage(s);
  Result := True;
end;

function TFoo.ShowTextConst(const s: string): Boolean;
begin
  Dialogs.ShowMessage(s);
  Result := True;
end;

function TFoo.ShowTextVar(var s: string): Boolean;
begin
  Dialogs.ShowMessage(s);
  s := s + 'abc';
  Result := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterLogging(TStringsLogging.Create(Memo1.Lines));
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
