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
  zShellViewModel,
  StdCtrls,
  ExtCtrls,
  ActnList,
  DSharp.Bindings.VCLControls;

type
  TShellView = class(TForm)
    Main: TPanel;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
