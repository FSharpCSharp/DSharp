object TasksView: TTasksView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object NewTaskItem: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Caption = 'NewTaskItem'
    FullRepaint = False
    TabOrder = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 254
    Width = 451
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
    object ItemsCount: TLabel
      Left = 10
      Top = 10
      Width = 56
      Height = 13
      Align = alLeft
      Caption = 'ItemsCount'
      Layout = tlCenter
    end
    object All: TButton
      Left = 241
      Top = 10
      Width = 100
      Height = 30
      Align = alRight
      Caption = 'All'
      TabOrder = 0
    end
    object Completed: TButton
      Left = 341
      Top = 10
      Width = 100
      Height = 30
      Align = alRight
      Caption = 'Completed'
      TabOrder = 1
    end
  end
  object Tasks: TScrollBox
    Left = 0
    Top = 50
    Width = 451
    Height = 204
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Constraints.MinHeight = 1
    TabOrder = 2
  end
end
