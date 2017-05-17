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
    object MovePrevious: TButton
      Left = 16
      Top = 176
      Width = 90
      Height = 25
      Caption = 'Previous'
      TabOrder = 2
    end
    object MoveNext: TButton
      Left = 121
      Top = 176
      Width = 88
      Height = 25
      Caption = 'Next'
      TabOrder = 3
    end
  end
  object AddContact: TButton
    Left = 199
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Add contact'
    TabOrder = 2
    OnClick = AddContactClick
  end
  object DeleteContact: TButton
    Left = 320
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Delete contact'
    TabOrder = 3
    OnClick = DeleteContactClick
  end
  object SelectFirst: TButton
    Left = 439
    Top = 8
    Width = 107
    Height = 25
    Caption = 'Select first'
    TabOrder = 4
  end
  object BindingGroup1: TBindingGroup
    Left = 272
    Top = 152
    Bindings = <
      item
        BindingMode = bmOneWay
        Source = Owner
        SourcePropertyName = 'Contacts'
        Target = ListBox1
        TargetPropertyName = 'View.ItemsSource'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.CurrentItem'
        Target = Panel1
        TargetPropertyName = 'BindingSource'
      end
      item
        Source = Panel1
        SourcePropertyName = 'BindingSource.Firstname'
        Target = Edit1
        TargetPropertyName = 'Text'
      end
      item
        Source = Panel1
        SourcePropertyName = 'BindingSource.Lastname'
        Target = Edit2
        TargetPropertyName = 'Text'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.CanMoveCurrentToNext'
        Target = MoveNext
        TargetPropertyName = 'Enabled'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.CanMoveCurrentToPrevious'
        Target = MovePrevious
        TargetPropertyName = 'Enabled'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.MoveCurrentToNext'
        Target = MoveNext
        TargetPropertyName = 'OnClick'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.MoveCurrentToPrevious'
        Target = MovePrevious
        TargetPropertyName = 'OnClick'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.MoveCurrentToFirst'
        Target = SelectFirst
        TargetPropertyName = 'OnClick'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.CurrentItem'
        Target = DeleteContact
        TargetPropertyName = 'Enabled'
      end
      item
        Source = ListBox1
        SourcePropertyName = 'View.CanMoveCurrentToPrevious'
        Target = SelectFirst
        TargetPropertyName = 'Enabled'
      end>
  end
end
