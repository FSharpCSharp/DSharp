object ShellView: TShellView
  Left = 0
  Top = 0
  ClientHeight = 460
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Trebuchet MS'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 35
  object ActiveItem: TPanel
    Left = 0
    Top = 61
    Width = 623
    Height = 399
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    Padding.Left = 80
    Padding.Top = 40
    Padding.Right = 80
    Padding.Bottom = 40
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 623
    Height = 61
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    DesignSize = (
      623
      61)
    object Label1: TLabel
      Left = 80
      Top = 0
      Width = 214
      Height = 61
      Caption = 'My Games'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -48
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
    end
    object Image1: TImage
      Left = 10
      Top = 7
      Width = 48
      Height = 48
      AutoSize = True
    end
    object Back: TImage
      Left = 10
      Top = 7
      Width = 48
      Height = 48
    end
    object BusyIndicator: TPanel
      Left = 432
      Top = 14
      Width = 177
      Height = 41
      Anchors = [akTop, akRight]
      Caption = 'Loading...'
      TabOrder = 0
      Visible = False
    end
  end
end
