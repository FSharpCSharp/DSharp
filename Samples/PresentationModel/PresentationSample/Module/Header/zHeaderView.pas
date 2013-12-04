unit zHeaderView;

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
  THeaderView = class(TFrame)
    DisplayName: TLabel;
  private
    { Private declarations }
  end;

var
  HeaderView: THeaderView;

implementation

{$R *.dfm}

initialization

THeaderView.ClassName;

end.
