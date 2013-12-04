object TaskViewView: TTaskViewView
  Left = 0
  Top = 0
  Width = 451
  Height = 50
  Align = alTop
  Color = clWhite
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  ParentColor = False
  TabOrder = 0
  object Bevel1: TBevel
    Left = 10
    Top = 40
    Width = 431
    Height = 10
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 358
    ExplicitWidth = 768
  end
  object Description: TLabel
    Left = 60
    Top = 10
    Width = 53
    Height = 13
    Align = alClient
    Caption = 'Description'
    Layout = tlCenter
  end
  object IsCompleted: TCheckBox
    Left = 10
    Top = 10
    Width = 50
    Height = 30
    TabStop = False
    Align = alLeft
    TabOrder = 0
  end
  object Remove: TButton
    Left = 366
    Top = 10
    Width = 75
    Height = 30
    Align = alRight
    Caption = 'Remove'
    TabOrder = 1
  end
end
