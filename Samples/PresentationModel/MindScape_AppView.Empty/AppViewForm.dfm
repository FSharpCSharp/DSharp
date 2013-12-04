object AppView: TAppView
  Left = 0
  Top = 0
  Caption = 'AppView'
  ClientHeight = 201
  ClientWidth = 193
  Color = clGradientActiveCaption
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
    Visible = False
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
    Visible = False
  end
  object IncrementCount: TButton
    Left = 113
    Top = 127
    Width = 75
    Height = 75
    Caption = '+'
    TabOrder = 3
    Visible = False
  end
  object DecrementCount: TButton
    Left = 32
    Top = 127
    Width = 75
    Height = 75
    Caption = '-'
    TabOrder = 2
    Visible = False
  end
  object Button1: TButton
    Left = 194
    Top = 47
    Width = 75
    Height = 75
    Action = MultipyByTwo
    TabOrder = 1
    Visible = False
  end
  object Button2: TButton
    Left = 194
    Top = 127
    Width = 75
    Height = 75
    Action = IncrementByTwo
    TabOrder = 4
    Visible = False
  end
  object IncrementValue: TTrackBar
    Left = 32
    Top = 264
    Width = 156
    Height = 25
    Max = 5
    Min = -5
    TabOrder = 5
    Visible = False
  end
  object IncrementCountByIncrementValue: TButton
    Left = 194
    Top = 208
    Width = 75
    Height = 75
    Caption = 'Inc'
    TabOrder = 6
    Visible = False
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
      end>
    Left = 216
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
