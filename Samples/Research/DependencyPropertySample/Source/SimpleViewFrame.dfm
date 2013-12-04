object SimpleView: TSimpleView
  Left = 0
  Top = 0
  Width = 1019
  Height = 623
  TabOrder = 0
  object PanelView: TPanel
    Left = 0
    Top = 0
    Width = 1019
    Height = 623
    Align = alClient
    Caption = 'PanelView'
    Color = clGradientActiveCaption
    ParentBackground = False
    TabOrder = 0
    object LabelDataContext: TLabel
      Left = 24
      Top = 32
      Width = 187
      Height = 29
      Caption = 'LabelDataContext'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ButtonShowInfo: TButton
      Left = 24
      Top = 152
      Width = 209
      Height = 25
      Caption = 'Show info'
      TabOrder = 0
      OnClick = ButtonShowInfoClick
    end
    object MemoLog: TMemo
      Left = 1
      Top = 256
      Width = 1017
      Height = 366
      Align = alBottom
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
    end
  end
end
