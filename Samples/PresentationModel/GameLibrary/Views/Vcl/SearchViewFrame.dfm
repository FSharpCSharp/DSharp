object SearchView: TSearchView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object SearchResults: TPanel
    Left = 0
    Top = 45
    Width = 451
    Height = 214
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 259
    Width = 451
    Height = 45
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    object AddGame: TButton
      Left = 10
      Top = 0
      Width = 143
      Height = 45
      Caption = 'Add Game'
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 45
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    object SearchText: TEdit
      Left = 1
      Top = 0
      Width = 272
      Height = 21
      TabOrder = 0
    end
    object ExecuteSearch: TButton
      Left = 288
      Top = 0
      Width = 89
      Height = 45
      Caption = 'Go'
      TabOrder = 1
    end
  end
end
