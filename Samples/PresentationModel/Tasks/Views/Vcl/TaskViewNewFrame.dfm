object TaskViewNew: TTaskViewNew
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
  object Add: TButton
    Left = 366
    Top = 10
    Width = 75
    Height = 30
    Align = alRight
    Caption = 'Add'
    TabOrder = 0
  end
  object Description: TEdit
    Left = 10
    Top = 10
    Width = 356
    Height = 30
    Align = alClient
    TabOrder = 1
    TextHint = 'What needs to be done?'
    ExplicitHeight = 21
  end
end
