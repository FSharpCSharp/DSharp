object ShellView: TShellView
  Left = 0
  Top = 0
  ClientHeight = 962
  ClientWidth = 1165
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object PanelLeftButtons: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 962
    Align = alLeft
    TabOrder = 0
    object ShowExampleOne: TButton
      Left = 24
      Top = 32
      Width = 150
      Height = 25
      Caption = 'Example One'
      TabOrder = 0
    end
    object ShowExampleTwo: TButton
      Left = 24
      Top = 87
      Width = 150
      Height = 25
      Caption = 'Example Two'
      TabOrder = 1
    end
  end
  object ActiveItem: TPanel
    Left = 201
    Top = 0
    Width = 964
    Height = 962
    Align = alClient
    BevelOuter = bvNone
    Caption = 'ActiveItem'
    DoubleBuffered = False
    FullRepaint = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 1
  end
end
