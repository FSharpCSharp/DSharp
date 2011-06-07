object Main: TMain
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
  object VirtualStringTree1: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 337
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
    Left = 376
    Top = 48
    Width = 121
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Lastname'
    TabOrder = 1
  end
  object Firstname: TLabeledEdit
    Left = 376
    Top = 104
    Width = 121
    Height = 21
    EditLabel.Width = 47
    EditLabel.Height = 13
    EditLabel.Caption = 'Firstname'
    TabOrder = 2
  end
  object TreeViewPresenter1: TTreeViewPresenter
    ColumnDefinitions = <
      item
        Caption = 'Lastname'
      end
      item
        Caption = 'Firstname'
      end>
    ListMode = True
    TreeView = VirtualStringTree1
    UseRtti = True
    Left = 136
    Top = 104
  end
  object BindingGroup1: TBindingGroup
    Left = 264
    Top = 104
    Bindings = <
      item
        Target = Lastname
        TargetPropertyName = 'Text'
        Source = TreeViewPresenter1
        SourcePropertyName = 'CurrentItem.Lastname'
      end
      item
        Target = Firstname
        TargetPropertyName = 'Text'
        Source = TreeViewPresenter1
        SourcePropertyName = 'CurrentItem.Firstname'
      end>
  end
end
