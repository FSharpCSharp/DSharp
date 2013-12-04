unit zSomeView;

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
  ExtCtrls,
  ActnList,
  DSharp.Bindings.VCLControls;

type
  TSomeView = class(TFrame)
    DisplayName: TLabel;
  private
    { Private declarations }
  end;

var
  SomeView: TSomeView;

implementation

{$R *.dfm}

initialization

TSomeView.ClassName;

end.
