unit CalculatorViewModel;

interface

uses
  CalculatorInterfaces,
  DSharp.PresentationModel.ViewModelBase,
  SysUtils;

type
  TCalcOperator = (Add, Subtract, Multiply, Divide);

  TCalculatorViewModel = class(TViewModelBase, ICalculatorViewModel)
  private
    FLeftOperand: Double;
    FRightOperand: Double;
    FCalcOperator: TCalcOperator;
    FCalcResult: Double;
    FError: string;
  public
    procedure Calculate;

    property LeftOperand: Double read FLeftOperand write FLeftOperand;
    property RightOperand: Double read FRightOperand write FRightOperand;
    property CalcOperator: TCalcOperator read FCalcOperator write FCalcOperator;
    property CalcResult: Double read FCalcResult write FCalcResult;
    property Error: string read FError write FError;
  end;

implementation

{ TCalculatorViewModel }

procedure TCalculatorViewModel.Calculate;
begin
  try
    case FCalcOperator of
      Add: FCalcResult := FLeftOperand + FRightOperand;
      Subtract: FCalcResult := FLeftOperand - FRightOperand;
      Multiply: FCalcResult := FLeftOperand * FRightOperand;
      Divide: FCalcResult := FLeftOperand / FRightOperand;
    end;
    FError := '';
  except
    on E: Exception do
    begin
      FError := E.Message;
      FCalcResult := 0;
    end;
  end;
  DoPropertyChanged('CalcResult');
  DoPropertyChanged('Error');
end;

initialization
  TCalculatorViewModel.ClassName;

end.
