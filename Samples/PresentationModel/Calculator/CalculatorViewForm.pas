unit CalculatorViewForm;

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
  DSharp.Bindings,
  DSharp.Bindings.VCLControls;

type
  TCalculatorView = class(TForm)
    CalcOperator: TComboBox;
    CalcResult: TEdit;
    Calculate: TButton;
    Label1: TLabel;
    LeftOperand: TEdit;
    RightOperand: TEdit;
    Error: TEdit;
  end;

implementation

{$R *.dfm}

initialization

TCalculatorView.ClassName;

end.
