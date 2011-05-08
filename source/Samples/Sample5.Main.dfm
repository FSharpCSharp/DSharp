object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sample 5'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 185
    Height = 274
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 199
    Top = 40
    Width = 347
    Height = 242
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 47
      Height = 13
      Caption = 'Firstname'
    end
    object Label2: TLabel
      Left = 176
      Top = 24
      Width = 46
      Height = 13
      Caption = 'Lastname'
    end
    object Edit1: TEdit
      Left = 16
      Top = 40
      Width = 145
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 176
      Top = 40
      Width = 145
      Height = 21
      TabOrder = 1
    end
  end
  object Button1: TButton
    Left = 199
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Add contact'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 320
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Delete contact'
    TabOrder = 3
    OnClick = Button2Click
  end
  object BindingGroup1: TBindingGroup
    Left = 272
    Top = 152
    Bindings = <
      item
        Target = ListBox1
        TargetPropertyName = 'ItemsSource'
        Source = Owner
        SourcePropertyName = 'Contacts'
      end
      item
        Target = Edit1
        TargetPropertyName = 'Text'
        Source = ListBox1
        SourcePropertyName = 'CurrentItem.Firstname'
      end
      item
        Target = Edit2
        TargetPropertyName = 'Text'
        Source = ListBox1
        SourcePropertyName = 'CurrentItem.Lastname'
      end>
  end
end
