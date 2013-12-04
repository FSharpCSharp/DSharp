object AppView: TAppView
  Left = 0
  Top = 0
  Caption = 'AppView'
  ClientHeight = 201
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 120
    Width = 111
    Height = 15
    Caption = 'Button'#39's description:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelDescription: TLabel
    Left = 24
    Top = 141
    Width = 94
    Height = 15
    Caption = 'LabelDescription'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object ClickMe: TButton
    Left = 24
    Top = 32
    Width = 75
    Height = 25
    Caption = 'ClickMe'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
end
