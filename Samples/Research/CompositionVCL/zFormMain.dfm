object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 462
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ActiveItem: TPanel
    Left = 0
    Top = 105
    Width = 496
    Height = 357
    Align = alClient
    Caption = 'ActiveItem'
    TabOrder = 0
  end
  object Header: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 105
    Align = alTop
    Caption = 'Composition Test'
    TabOrder = 1
    object ActivateFirst: TButton
      Left = 16
      Top = 16
      Width = 105
      Height = 25
      Caption = 'ActivateFirst'
      TabOrder = 0
      OnClick = ActivateFirstClick
    end
    object FreeFirst: TButton
      Left = 16
      Top = 47
      Width = 105
      Height = 25
      Caption = 'FreeFirst'
      TabOrder = 1
      OnClick = FreeFirstClick
    end
  end
  object MemoLog: TMemo
    Left = 496
    Top = 105
    Width = 356
    Height = 357
    Align = alRight
    BorderStyle = bsNone
    TabOrder = 2
  end
end
