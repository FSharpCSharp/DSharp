object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Event Aggregator Sample'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    Left = 250
    Top = 0
    Width = 534
    Height = 562
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 185
    ExplicitWidth = 667
    ExplicitHeight = 384
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 562
    Align = alLeft
    TabOrder = 1
    DesignSize = (
      250
      562)
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 145
      Height = 25
      Action = Action1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 16
      Top = 47
      Width = 145
      Height = 25
      Action = Action2
      TabOrder = 1
    end
    object Memo1: TMemo
      Left = 1
      Top = 88
      Width = 248
      Height = 473
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      ScrollBars = ssVertical
      TabOrder = 2
      ExplicitWidth = 183
      ExplicitHeight = 373
    end
  end
  object ActionList1: TActionList
    Left = 56
    Top = 200
    object Action1: TAction
      Caption = '&1: send TCustomerMessage'
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Caption = '&2: TLogMessage'
      OnExecute = Action2Execute
    end
  end
end
