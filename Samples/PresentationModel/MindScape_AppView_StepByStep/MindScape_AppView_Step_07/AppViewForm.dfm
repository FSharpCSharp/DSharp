object AppView: TAppView
  Left = 0
  Top = 0
  Caption = 'AppView'
  ClientHeight = 201
  ClientWidth = 223
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -48
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 65
  object Count: TEdit
    Left = 32
    Top = 48
    Width = 156
    Height = 73
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = 'Count'
  end
  object IncrementCount: TButton
    Left = 113
    Top = 127
    Width = 75
    Height = 75
    Caption = '+'
    TabOrder = 2
  end
  object DecrementCount: TButton
    Left = 32
    Top = 127
    Width = 75
    Height = 75
    Caption = '-'
    TabOrder = 1
  end
end
