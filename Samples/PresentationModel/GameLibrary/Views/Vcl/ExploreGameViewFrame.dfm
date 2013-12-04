object ExploreGameView: TExploreGameView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object Game: TPanel
    Left = 0
    Top = 0
    Width = 673
    Height = 320
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Game'
    Constraints.MinHeight = 100
    Constraints.MinWidth = 1
    FullRepaint = False
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 451
  end
  object Panel1: TPanel
    Left = 0
    Top = 420
    Width = 673
    Height = 79
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    Visible = False
    ExplicitWidth = 451
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 48
      Height = 13
      Align = alTop
      Caption = 'Borrower:'
    end
    object Borrower: TEdit
      Left = 0
      Top = 13
      Width = 673
      Height = 21
      Margins.Bottom = 0
      Align = alTop
      TabOrder = 0
      Text = 'Borrower'
      ExplicitWidth = 451
    end
    object Panel4: TPanel
      Left = 0
      Top = 34
      Width = 673
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel3'
      FullRepaint = False
      ShowCaption = False
      TabOrder = 1
      ExplicitWidth = 451
      object CheckOut: TButton
        Left = 473
        Top = 0
        Width = 200
        Height = 45
        Align = alRight
        Caption = 'Check Out'
        TabOrder = 0
        ExplicitLeft = 251
      end
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 362
    Width = 673
    Height = 58
    Margins.Left = 0
    Margins.Top = 42
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    Visible = False
    ExplicitWidth = 451
    object BorrowedMessage: TLabel
      Left = 0
      Top = 0
      Width = 88
      Height = 13
      Align = alTop
      Caption = 'BorrowedMessage'
    end
    object Panel3: TPanel
      Left = 0
      Top = 13
      Width = 673
      Height = 45
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel3'
      FullRepaint = False
      ShowCaption = False
      TabOrder = 0
      ExplicitWidth = 451
      object CheckIn: TButton
        Left = 473
        Top = 0
        Width = 200
        Height = 45
        Align = alRight
        Caption = 'Check In'
        TabOrder = 0
        ExplicitLeft = 251
      end
    end
  end
end
