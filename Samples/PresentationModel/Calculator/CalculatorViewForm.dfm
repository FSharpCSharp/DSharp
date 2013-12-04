object CalculatorView: TCalculatorView
  Left = 0
  Top = 0
  Caption = 'CalculatorView'
  ClientHeight = 97
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 383
    Top = 19
    Width = 8
    Height = 13
    Caption = '='
  end
  object LeftOperand: TEdit
    Left = 16
    Top = 16
    Width = 105
    Height = 21
    TabOrder = 0
    Text = 'LeftOperand'
  end
  object RightOperand: TEdit
    Left = 256
    Top = 16
    Width = 105
    Height = 21
    TabOrder = 2
    Text = 'RightOperand'
  end
  object CalcOperator: TComboBox
    Left = 143
    Top = 16
    Width = 90
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'Add'
      'Subtract'
      'Multiply'
      'Divide')
  end
  object CalcResult: TEdit
    Left = 408
    Top = 16
    Width = 105
    Height = 21
    TabOrder = 3
    Text = 'Result'
  end
  object Calculate: TButton
    Left = 16
    Top = 56
    Width = 105
    Height = 25
    Caption = 'Calculate'
    Default = True
    TabOrder = 4
  end
  object Error: TEdit
    Left = 143
    Top = 58
    Width = 370
    Height = 21
    TabOrder = 5
    Text = 'Error'
  end
end
