object MainView: TMainView
  Left = 0
  Top = 0
  Caption = 'MainView'
  ClientHeight = 301
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 185
    Top = 0
    Height = 301
  end
  object Navigation: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 301
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Navigation'
    TabOrder = 0
  end
  object WorkingArea: TPanel
    Left = 188
    Top = 0
    Width = 374
    Height = 301
    Align = alClient
    BevelOuter = bvNone
    Caption = 'WorkingArea'
    TabOrder = 1
  end
end
