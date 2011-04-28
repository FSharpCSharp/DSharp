object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sample 1'
  ClientHeight = 290
  ClientWidth = 554
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
  object Edit1: TEdit
    Left = 16
    Top = 16
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 16
    Top = 56
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object ColorBox1: TColorBox
    Left = 208
    Top = 16
    Width = 145
    Height = 22
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 208
    Top = 58
    Width = 145
    Height = 17
    Caption = 'Enable color picker'
    TabOrder = 3
  end
  object ComboBox1: TComboBox
    Left = 208
    Top = 96
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    Items.Strings = (
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '14'
      '16'
      '18')
  end
  object Button1: TButton
    Left = 16
    Top = 136
    Width = 105
    Height = 25
    Caption = 'Change caption'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Edit3: TEdit
    Left = 16
    Top = 96
    Width = 145
    Height = 21
    TabOrder = 6
  end
end
