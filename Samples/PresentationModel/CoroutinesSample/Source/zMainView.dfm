object MainView: TMainView
  Left = 0
  Top = 0
  ClientHeight = 484
  ClientWidth = 649
  Color = clWindow
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Results: TMemo
    Left = 185
    Top = 0
    Width = 464
    Height = 484
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 484
    Align = alLeft
    TabOrder = 1
    object AddGame: TButton
      Left = 32
      Top = 26
      Width = 120
      Height = 33
      Caption = 'AddGame'
      TabOrder = 0
    end
    object Button1: TButton
      Left = 32
      Top = 65
      Width = 120
      Height = 25
      Action = ShowGame
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 328
    Top = 160
    object ShowGame: TAction
      Caption = 'ShowGame'
    end
  end
end
