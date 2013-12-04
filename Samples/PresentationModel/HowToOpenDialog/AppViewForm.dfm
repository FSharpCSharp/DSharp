object AppView: TAppView
  Left = 0
  Top = 0
  Caption = 'AppView'
  ClientHeight = 201
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Description: TLabel
    Left = 24
    Top = 32
    Width = 63
    Height = 15
    Caption = 'Description'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Button1: TButton
    Left = 24
    Top = 64
    Width = 113
    Height = 25
    Action = ActionOpen1
    Caption = 'Open dialog &1'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Button3: TButton
    Left = 208
    Top = 168
    Width = 87
    Height = 25
    Action = ActionClose
    Caption = 'Close &App'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object Button2: TButton
    Left = 24
    Top = 95
    Width = 113
    Height = 25
    Action = ActionOpen2
    Caption = 'Open dialog &2'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object ActionList1: TActionList
    Left = 184
    Top = 48
    object ActionClose: TAction
      Caption = 'Close App'
      ShortCut = 27
    end
    object ActionOpen1: TAction
      Caption = 'Open dialog 1'
    end
    object ActionOpen2: TAction
      Caption = 'Open dialog 2'
    end
  end
end
