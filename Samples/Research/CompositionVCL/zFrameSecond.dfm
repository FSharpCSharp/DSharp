object FrameSecond: TFrameSecond
  Left = 0
  Top = 0
  Width = 300
  Height = 250
  TabOrder = 0
  object SecondHeader: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 41
    Align = alTop
    Caption = 'SecondHeader'
    TabOrder = 0
    ExplicitWidth = 320
  end
  object SecondFooter: TPanel
    Left = 0
    Top = 209
    Width = 300
    Height = 41
    Align = alBottom
    Caption = 'SecondFooter'
    TabOrder = 1
    ExplicitTop = 199
    ExplicitWidth = 320
  end
  object SecondContent: TPanel
    Left = 0
    Top = 41
    Width = 300
    Height = 168
    Align = alClient
    Caption = 'SecondContent'
    TabOrder = 2
    ExplicitWidth = 320
    ExplicitHeight = 158
  end
end
