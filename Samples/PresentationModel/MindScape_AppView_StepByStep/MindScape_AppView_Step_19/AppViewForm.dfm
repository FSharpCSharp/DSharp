object AppView: TAppView
  Left = 0
  Top = 0
  Caption = 'AppView'
  ClientHeight = 291
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -48
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 65
  object IncVal: TLabel
    Left = 32
    Top = 212
    Width = 156
    Height = 65
    Alignment = taCenter
    AutoSize = False
    Caption = 'IncVal'
  end
  object Count: TEdit
    Left = 32
    Top = 48
    Width = 156
    Height = 73
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
    Text = 'Count'
  end
  object IncrementCount: TButton
    Left = 113
    Top = 127
    Width = 75
    Height = 75
    Caption = '+'
    TabOrder = 3
  end
  object DecrementCount: TButton
    Left = 32
    Top = 127
    Width = 75
    Height = 75
    Caption = '-'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 194
    Top = 47
    Width = 75
    Height = 75
    Action = MultipyByTwo
    TabOrder = 1
  end
  object Button2: TButton
    Left = 194
    Top = 127
    Width = 75
    Height = 75
    Action = IncrementByTwo
    TabOrder = 4
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 303
    Height = 23
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 23
    Width = 303
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager1
    Caption = 'ActionMainMenuBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    PersistentHotKeys = True
    Spacing = 0
  end
  object IncrementValue: TTrackBar
    Left = 32
    Top = 264
    Width = 156
    Height = 25
    Max = 5
    Min = -5
    TabOrder = 7
  end
  object IncrementCountByIncrementValue: TButton
    Left = 194
    Top = 208
    Width = 75
    Height = 75
    Caption = 'Inc'
    TabOrder = 8
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = MultipyByTwo
            Caption = '*&2'
          end
          item
            Action = IncrementByTwo
          end>
        ActionBar = ActionToolBar1
      end
      item
      end
      item
        Items = <
          item
            Action = MultipyByTwo
            Caption = '*&2'
          end
          item
            Action = IncrementByTwo
          end
          item
            Items = <
              item
                Action = MultipyByTwo
                Caption = '*&2'
              end
              item
                Action = IncrementByTwo
              end>
            Caption = '&Count'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    Left = 128
    StyleName = 'Platform Default'
    object MultipyByTwo: TAction
      Category = 'Count'
      Caption = '*2'
    end
    object IncrementByTwo: TAction
      Category = 'Count'
      Caption = '+2'
    end
  end
end
