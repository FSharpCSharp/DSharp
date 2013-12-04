object GalleryView: TGalleryView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 647
  ExplicitHeight = 604
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 60
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 647
    object Label1: TLabel
      Left = 16
      Top = 12
      Width = 54
      Height = 38
      Caption = '500'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -33
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object ComboBox1: TComboBox
      Left = 160
      Top = 20
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Popular'
      Items.Strings = (
        'Popular'
        'Fresh'
        'Upcoming')
    end
    object ComboBox2: TComboBox
      Left = 343
      Top = 20
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'All Categories'
      Items.Strings = (
        'All Categories'
        'People'
        'Animals'
        'Nature')
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 60
    Width = 451
    Height = 244
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 1
    ExplicitWidth = 647
    ExplicitHeight = 544
    object PhotoCollection: TFlowPanel
      Left = 0
      Top = 0
      Width = 647
      Height = 544
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'PhotoCollection'
      FlowStyle = fsTopBottomLeftRight
      FullRepaint = False
      ShowCaption = False
      TabOrder = 0
      TabStop = True
    end
  end
end
