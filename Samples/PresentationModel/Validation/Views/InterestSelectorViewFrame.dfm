object InterestSelectorView: TInterestSelectorView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object InterestSelectorErrorIndicator: TLabel
    Left = 0
    Top = 290
    Width = 68
    Height = 14
    Align = alBottom
    Caption = 'Error Indicator'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Interests: TScrollBox
    Left = 0
    Top = 0
    Width = 451
    Height = 290
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
    ExplicitHeight = 304
  end
end
