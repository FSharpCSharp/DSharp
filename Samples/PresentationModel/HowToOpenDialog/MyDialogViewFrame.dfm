object MyDialogView: TMyDialogView
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object DisplayName: TLabel
    Left = 26
    Top = 24
    Width = 108
    Height = 23
    Caption = 'DisplayName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object OK: TButton
    Left = 26
    Top = 136
    Width = 75
    Height = 25
    Action = ActionOK
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Value: TEdit
    Left = 26
    Top = 72
    Width = 279
    Height = 23
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = 'Value'
  end
  object Cancel: TButton
    Left = 107
    Top = 136
    Width = 75
    Height = 25
    Action = ActionCancel
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 224
    Top = 24
    object ActionOK: TAction
      Caption = 'OK'
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      ShortCut = 27
    end
  end
end
