object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 362
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 220
    Top = 0
    Width = 364
    Height = 362
    Align = alClient
    Lines.Strings = (
      'Log:'
      '')
    TabOrder = 0
    ExplicitWidth = 632
    ExplicitHeight = 400
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 220
    Height = 362
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitHeight = 462
    object ButtonAdd_Hi: TButton
      Left = 16
      Top = 16
      Width = 180
      Height = 25
      Caption = 'Add proc 1: Hi instance proc'
      TabOrder = 0
      OnClick = ButtonAdd_HiClick
    end
    object ButtonInvoke: TButton
      Left = 16
      Top = 169
      Width = 180
      Height = 25
      Caption = 'Invoke Events'
      TabOrder = 1
      OnClick = ButtonInvokeClick
    end
    object ButtonAdd_Anonymous: TButton
      Left = 16
      Top = 47
      Width = 180
      Height = 25
      Caption = 'Add proc 2: anonymous method'
      TabOrder = 2
      OnClick = ButtonAdd_AnonymousClick
    end
    object ButtonAdd_ClassHi: TButton
      Left = 16
      Top = 78
      Width = 180
      Height = 25
      Caption = 'Add proc 3: ClassHi class proc'
      TabOrder = 3
      OnClick = ButtonAdd_ClassHiClick
    end
    object Button5: TButton
      Left = 16
      Top = 293
      Width = 180
      Height = 25
      Caption = 'Add'
      TabOrder = 4
      OnClick = Button5Click
    end
    object ButtonClear: TButton
      Left = 16
      Top = 200
      Width = 180
      Height = 25
      Caption = 'Clear Events'
      TabOrder = 5
      OnClick = ButtonClearClick
    end
  end
end
