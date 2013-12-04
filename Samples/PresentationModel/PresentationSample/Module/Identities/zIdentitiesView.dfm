object IdentitiesView: TIdentitiesView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Color = clWindow
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object Label1: TLabel
    Left = 40
    Top = 32
    Width = 175
    Height = 32
    Caption = 'Identities View'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 40
    Top = 160
    Width = 175
    Height = 32
    Caption = 'Identities View'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 186
    Top = 0
    Width = 265
    Height = 304
    Align = alRight
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 778
    ExplicitHeight = 827
    object Button1: TButton
      Left = 1
      Top = 1
      Width = 263
      Height = 33
      Action = ActionShowSelectedAccount
      Align = alTop
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Left = 576
    Top = 264
    object ActionShowSelectedAccount: TAction
      Caption = 'Show Account'
    end
  end
end
