object AddGameView: TAddGameView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  AutoSize = True
  TabOrder = 0
  ExplicitWidth = 482
  ExplicitHeight = 207
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 24
    Height = 13
    Align = alTop
    Caption = 'Title:'
  end
  object Label2: TLabel
    Left = 0
    Top = 34
    Width = 35
    Height = 13
    Align = alTop
    Caption = 'Rating:'
  end
  object Rating: TLabel
    Left = 0
    Top = 47
    Width = 12
    Height = 13
    Align = alTop
    Caption = '...'
  end
  object Label3: TLabel
    Left = 0
    Top = 60
    Width = 32
    Height = 13
    Align = alTop
    Caption = 'Notes:'
  end
  object Notes: TMemo
    Left = 0
    Top = 73
    Width = 482
    Height = 89
    Align = alTop
    TabOrder = 0
  end
  object AddGame: TButton
    Left = 0
    Top = 162
    Width = 482
    Height = 45
    Align = alTop
    BiDiMode = bdLeftToRight
    Caption = 'Add'
    Constraints.MaxWidth = 482
    ParentBiDiMode = False
    TabOrder = 1
  end
  object Title: TEdit
    Left = 0
    Top = 13
    Width = 482
    Height = 21
    Align = alTop
    TabOrder = 2
  end
end
