object EditElementsView: TEditElementsView
  Left = 0
  Top = 0
  Caption = 'EditElementsView'
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
  object AvailableElements: TListBox
    Left = 8
    Top = 8
    Width = 201
    Height = 274
    DragMode = dmAutomatic
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = AvailableElementsDblClick
    OnDragDrop = AvailableElementsDragDrop
    OnDragOver = AvailableElementsDragOver
  end
  object btnAddElements: TButton
    Left = 240
    Top = 104
    Width = 75
    Height = 25
    Action = AddElements
    TabOrder = 1
  end
  object btnRemoveElements: TButton
    Left = 240
    Top = 144
    Width = 75
    Height = 25
    Action = RemoveElements
    TabOrder = 2
  end
  object SelectedElements: TListBox
    Left = 345
    Top = 8
    Width = 201
    Height = 274
    ItemHeight = 13
    TabOrder = 3
    OnDblClick = SelectedElementsDblClick
    OnDragDrop = SelectedElementsDragDrop
    OnDragOver = SelectedElementsDragOver
  end
  object BindingGroup1: TBindingGroup
    Left = 272
    Top = 200
    Bindings = <>
  end
  object ActionList1: TActionList
    Left = 264
    Top = 40
    object AddElements: TAction
      Caption = 'Add'
    end
    object RemoveElements: TAction
      Caption = 'Remove'
    end
  end
end
