object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Sample 4'
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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Trigger event'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 538
    Height = 243
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end
