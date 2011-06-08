object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'VirtualTreeviewSample'
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
  object Contacts: TVirtualStringTree
    Left = 8
    Top = 48
    Width = 297
    Height = 185
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    NodeDataSize = 4
    TabOrder = 0
    Columns = <>
  end
  object Lastname: TLabeledEdit
    Left = 336
    Top = 64
    Width = 121
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Lastname'
    TabOrder = 1
  end
  object Firstname: TLabeledEdit
    Left = 336
    Top = 120
    Width = 121
    Height = 21
    EditLabel.Width = 47
    EditLabel.Height = 13
    EditLabel.Caption = 'Firstname'
    TabOrder = 2
  end
  object AddContact: TButton
    Left = 8
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Add contact'
    TabOrder = 3
    OnClick = AddContactClick
  end
  object DeleteContact: TButton
    Left = 128
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Delete contact'
    TabOrder = 4
    OnClick = DeleteContactClick
  end
  object ContactsPresenter: TTreeViewPresenter
    ColumnDefinitions = <
      item
        Caption = 'Lastname'
      end
      item
        Caption = 'Firstname'
      end>
    ListMode = True
    TreeView = Contacts
    UseRtti = True
    Left = 80
    Top = 80
  end
  object BindingGroup: TBindingGroup
    Left = 192
    Top = 80
    Bindings = <
      item
        Target = Lastname
        TargetPropertyName = 'Text'
        Source = ContactsPresenter
        SourcePropertyName = 'CurrentItem.Lastname'
      end
      item
        Target = Firstname
        TargetPropertyName = 'Text'
        Source = ContactsPresenter
        SourcePropertyName = 'CurrentItem.Firstname'
      end>
  end
end
