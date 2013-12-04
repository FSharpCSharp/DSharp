object GameDTOView: TGameDTOView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 621
  ExplicitHeight = 307
  object Label2: TLabel
    Left = 0
    Top = 129
    Width = 35
    Height = 13
    Align = alTop
    Caption = 'Rating:'
  end
  object Rating: TLabel
    Left = 0
    Top = 142
    Width = 12
    Height = 13
    Align = alTop
    Caption = '...'
  end
  object Label3: TLabel
    Left = 0
    Top = 155
    Width = 32
    Height = 13
    Align = alTop
    Caption = 'Notes:'
  end
  object Notes: TMemo
    Left = 0
    Top = 168
    Width = 621
    Height = 89
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 333
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 621
    Height = 129
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    FullRepaint = False
    ShowCaption = False
    TabOrder = 1
    ExplicitWidth = 333
    object Title: TLabel
      Left = 543
      Top = 0
      Width = 78
      Height = 46
      Align = alTop
      Alignment = taRightJustify
      Caption = 'Title'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -37
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
    end
    object Panel2: TPanel
      Left = 0
      Top = 46
      Width = 621
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel2'
      FullRepaint = False
      ShowCaption = False
      TabOrder = 0
      ExplicitWidth = 333
      object AddedOn: TLabel
        Left = 576
        Top = 0
        Width = 45
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        Caption = 'AddedOn'
      end
      object Label1: TLabel
        Left = 527
        Top = 0
        Width = 49
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        Caption = 'Added on '
      end
    end
  end
end
