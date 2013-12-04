unit CalculatorViewModel;

interface

uses
  SysUtils,
  CalculatorInterfaces,
  DSharp.PresentationModel.ViewModelBase;

type
  TCalcOperator = (Add, Subtract, Multiply, Divide);

  TCalculatorViewModel = class(TViewModel, ICalculatorViewModel)
  strict private
    FLeftOperand: Double;
    FRightOperand: Double;
    FCalcOperator: TCalcOperator;
    FCalcResult: Double;
    FError: string;
  strict protected
    function GetCalcOperator(): TCalcOperator; virtual;
    function GetCalcResult(): Double; virtual;
    function GetError(): string; virtual;
    function GetLeftOperand(): Double; virtual;
    function GetRightOperand(): Double; virtual;
    procedure SetCalcOperator(const Value: TCalcOperator); virtual;
    procedure SetCalcResult(const Value: Double); virtual;
    procedure SetError(const Value: string); virtual;
    procedure SetLeftOperand(const Value: Double); virtual;
    procedure SetRightOperand(const Value: Double); virtual;
  public
    procedure Calculate;

    property LeftOperand: Double read GetLeftOperand write SetLeftOperand;
    property RightOperand: Double read GetRightOperand write SetRightOperand;
    property CalcOperator: TCalcOperator read GetCalcOperator
      write SetCalcOperator;
    property CalcResult: Double read GetCalcResult write SetCalcResult;
    property Error: string read GetError write SetError;
  end;

implementation

{ TCalculatorViewModel }

procedure TCalculatorViewModel.Calculate;
begin
  try
    case FCalcOperator of
      Add:
        FCalcResult := FLeftOperand + FRightOperand;
      Subtract:
        FCalcResult := FLeftOperand - FRightOperand;
      Multiply:
        FCalcResult := FLeftOperand * FRightOperand;
      Divide:
        FCalcResult := FLeftOperand / FRightOperand;
    end;
    FError := '';
  except
    on E: Exception do
    begin
      FError := E.Message;
      FCalcResult := 0;
    end;
  end;
  NotifyOfPropertyChange('CalcResult');
  NotifyOfPropertyChange('Error');
end;

function TCalculatorViewModel.GetCalcOperator(): TCalcOperator;
begin
  Result := FCalcOperator;
end;

function TCalculatorViewModel.GetCalcResult(): Double;
begin
  Result := FCalcResult;
end;

function TCalculatorViewModel.GetError(): string;
begin
  Result := FError;
end;

function TCalculatorViewModel.GetLeftOperand(): Double;
begin
  Result := FLeftOperand;
end;

function TCalculatorViewModel.GetRightOperand(): Double;
begin
  Result := FRightOperand;
end;

procedure TCalculatorViewModel.SetCalcOperator(const Value: TCalcOperator);
begin
  FCalcOperator := Value;
end;

procedure TCalculatorViewModel.SetCalcResult(const Value: Double);
begin
  FCalcResult := Value;
end;

procedure TCalculatorViewModel.SetError(const Value: string);
begin
  FError := Value;
end;

procedure TCalculatorViewModel.SetLeftOperand(const Value: Double);
begin
  FLeftOperand := Value;
end;

procedure TCalculatorViewModel.SetRightOperand(const Value: Double);
begin
  FRightOperand := Value;
end;

initialization

TCalculatorViewModel.ClassName;

end.
