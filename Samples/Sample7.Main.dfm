object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sample7'
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
  OnDestroy = FormDestroy
  DesignSize = (
    554
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'List items'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 538
    Height = 243
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object Button2: TButton
    Left = 128
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Custom filter'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 248
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'B'
  end
end
