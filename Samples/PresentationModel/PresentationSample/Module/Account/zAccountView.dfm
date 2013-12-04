object AccountView: TAccountView
  Left = 0
  Top = 0
  Width = 937
  Height = 590
  Align = alCustom
  TabOrder = 0
  DesignSize = (
    937
    590)
  object AccountName: TLabel
    Left = 40
    Top = 40
    Width = 164
    Height = 32
    Caption = 'Account View'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object ActiveState: TPanel
    Left = 40
    Top = 96
    Width = 843
    Height = 440
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 0
  end
  object Button1: TButton
    Left = 320
    Top = 48
    Width = 100
    Height = 25
    Action = ActionDetails
    TabOrder = 1
  end
  object Button2: TButton
    Left = 449
    Top = 48
    Width = 100
    Height = 25
    Action = ActionAuthorizations
    TabOrder = 2
  end
  object Button3: TButton
    Left = 570
    Top = 48
    Width = 100
    Height = 25
    Action = ActionGroups
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 536
    Top = 160
    object ActionDetails: TAction
      Caption = 'Details'
    end
    object ActionAuthorizations: TAction
      Caption = 'Authorizations'
    end
    object ActionGroups: TAction
      Caption = 'Groups'
    end
  end
end
