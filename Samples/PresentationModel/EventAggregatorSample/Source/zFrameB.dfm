object FrameB: TFrameB
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 250
    Height = 263
    Align = alClient
    BorderStyle = bsNone
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 451
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 41
    Align = alTop
    Caption = 'Frame B'
    TabOrder = 1
    ExplicitWidth = 451
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Send to &A'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
