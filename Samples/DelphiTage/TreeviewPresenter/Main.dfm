object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
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
    Left = 0
    Top = 41
    Width = 554
    Height = 183
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 0
    Columns = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 41
    Align = alTop
    TabOrder = 1
    object Edit1: TEdit
      Left = 16
      Top = 11
      Width = 177
      Height = 21
      TabOrder = 0
      OnChange = Edit1Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 224
    Width = 554
    Height = 66
    Align = alBottom
    TabOrder = 2
    object LabeledEdit1: TLabeledEdit
      Left = 16
      Top = 24
      Width = 233
      Height = 21
      EditLabel.Width = 75
      EditLabel.Height = 13
      EditLabel.Caption = 'Company Name'
      TabOrder = 0
    end
  end
  object TreeViewPresenter1: TTreeViewPresenter
    TreeView = VirtualStringTree1
    Left = 96
    Top = 48
    ColumnDefinitions = <
      item
        Caption = 'CustomerId'
        TextPropertyName = 'CustomerId'
        Width = 50
      end
      item
        Caption = 'CompanyName'
        TextPropertyName = 'CompanyName'
        Width = 200
      end
      item
        Caption = 'ContactName'
        TextPropertyName = 'ContactName'
        Width = 200
      end>
  end
  object BindingGroup1: TBindingGroup
    Left = 224
    Top = 48
    Bindings = <
      item
        Source = TreeViewPresenter1
        SourcePropertyName = 'SelectedItem.CompanyName'
        Target = LabeledEdit1
        TargetPropertyName = 'Text'
      end>
  end
end
