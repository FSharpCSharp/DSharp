object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sample6'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    554
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 538
    Height = 243
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Execute function'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 128
    Top = 8
    Width = 105
    Height = 25
    Caption = 'List expression tree'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 248
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Change expression tree'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 392
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Member expression'
    TabOrder = 4
    OnClick = Button4Click
  end
end
