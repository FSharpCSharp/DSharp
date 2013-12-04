object ShellView: TShellView
  Left = 0
  Top = 0
  ClientHeight = 699
  ClientWidth = 779
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 779
    Height = 25
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object ShowPageOne: TButton
      Left = 296
      Top = 0
      Width = 100
      Height = 25
      Caption = 'Show Page One'
      TabOrder = 0
    end
    object ShowPageTwo: TButton
      Left = 402
      Top = 0
      Width = 100
      Height = 25
      Caption = 'Show Page Two'
      TabOrder = 1
    end
  end
  object ActiveItem: TPanel
    Left = 0
    Top = 25
    Width = 779
    Height = 674
    Align = alClient
    BevelOuter = bvNone
    Caption = 'ActiveItem'
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 272
    ExplicitTop = 360
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
end
