object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 562
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 562
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      185
      562)
    object ButtonCreateFrame: TButton
      Left = 16
      Top = 477
      Width = 150
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Create Frame'
      TabOrder = 0
      OnClick = ButtonCreateFrameClick
    end
    object ButtonFreeFrame: TButton
      Left = 16
      Top = 508
      Width = 150
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Free Frame'
      TabOrder = 1
      OnClick = ButtonFreeFrameClick
    end
    object ButtonChangeDataContext: TButton
      Left = 16
      Top = 89
      Width = 150
      Height = 25
      Caption = 'Change DataContext'
      TabOrder = 2
      OnClick = ButtonChangeDataContextClick
    end
  end
  object PanelContent: TPanel
    Left = 185
    Top = 0
    Width = 599
    Height = 562
    Align = alClient
    Caption = 'PanelContent'
    TabOrder = 1
  end
end
