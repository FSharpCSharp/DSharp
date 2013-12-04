unit zShellView;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  DSharp.Bindings.VCLControls;

type
  TShellView = class(TForm)
    Name: TEdit;
    SayHello: TButton;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
