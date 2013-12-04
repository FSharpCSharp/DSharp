object MainView: TMainView
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Color = clGradientInactiveCaption
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  ExplicitWidth = 798
  ExplicitHeight = 600
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 120
    Height = 600
    Align = alLeft
    AutoSize = True
    BevelOuter = bvNone
    Color = 10114859
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    object LabelInfo: TLabel
      Left = 0
      Top = 585
      Width = 9
      Height = 15
      Align = alBottom
      Caption = '...'
    end
    object ActionClose: TLabel
      Left = 0
      Top = 530
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Close'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = -4
      ExplicitTop = 329
      ExplicitWidth = 263
    end
    object ActionAccount: TLabel
      Left = 0
      Top = 600
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Account'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 8
      ExplicitTop = 464
    end
    object ActionSaveAs: TLabel
      Left = 0
      Top = 370
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Save As'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = -4
      ExplicitTop = -2
      ExplicitWidth = 263
    end
    object ActionPrint: TLabel
      Left = 0
      Top = 410
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Print'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 33
      ExplicitTop = 265
      ExplicitWidth = 263
    end
    object ActionShare: TLabel
      Left = 0
      Top = 450
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Share'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 25
      ExplicitTop = 305
      ExplicitWidth = 263
    end
    object ActionExport: TLabel
      Left = 0
      Top = 490
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Export'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 10
      ExplicitTop = 321
      ExplicitWidth = 263
    end
    object ActionInfo: TLabel
      Left = 0
      Top = 210
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Info'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = -4
      ExplicitTop = -2
      ExplicitWidth = 263
    end
    object ActionNew: TLabel
      Left = 0
      Top = 250
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          New'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 51
      ExplicitTop = 41
      ExplicitWidth = 263
    end
    object ActionOpen: TLabel
      Left = 0
      Top = 290
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Open'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 2
      ExplicitTop = 84
      ExplicitWidth = 263
    end
    object ActionSave: TLabel
      Left = 0
      Top = 330
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Save'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 10
      ExplicitTop = 321
      ExplicitWidth = 263
    end
    object ActionOptions: TLabel
      Left = 0
      Top = 640
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Options'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitTop = 502
    end
    object Label1: TLabel
      Left = 0
      Top = 570
      Width = 120
      Height = 30
      Align = alTop
      AutoSize = False
      Caption = '          _______________'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11889982
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = 1
      ExplicitTop = 401
      ExplicitWidth = 118
    end
    object Label2: TLabel
      Left = 0
      Top = 180
      Width = 120
      Height = 30
      Align = alTop
      AutoSize = False
      Caption = '          _______________'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11889982
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = -6
      ExplicitTop = 64
    end
    object ActionIdentities: TLabel
      Left = 0
      Top = 140
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Identities'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitTop = 37
    end
    object ActionDocuments: TLabel
      Left = 0
      Top = 100
      Width = 120
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = '          Documents'
      Layout = tlCenter
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
      ExplicitLeft = -6
      ExplicitTop = -3
    end
    object Label3: TLabel
      Left = 0
      Top = 0
      Width = 120
      Height = 100
      Align = alTop
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11889982
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      OnMouseMove = OnItemMouseMove
      OnMouseLeave = OnItemMouseLeave
    end
  end
  object Panel2: TPanel
    Left = 120
    Top = 0
    Width = 678
    Height = 600
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Header: TPanel
      Left = 0
      Top = 0
      Width = 678
      Height = 120
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Header'
      Color = clWindow
      ParentBackground = False
      TabOrder = 0
    end
    object ActiveItem: TPanel
      Left = 0
      Top = 120
      Width = 678
      Height = 480
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
    end
  end
end
