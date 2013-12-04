object ResultsView: TResultsView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 320
  ExplicitHeight = 240
  object StatusMessage: TGroupBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    Caption = 'StatusMessage'
    Padding.Left = 20
    Padding.Top = 20
    Padding.Right = 20
    Padding.Bottom = 20
    TabOrder = 0
    object Results: TScrollBox
      Left = 22
      Top = 35
      Width = 276
      Height = 183
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
end
