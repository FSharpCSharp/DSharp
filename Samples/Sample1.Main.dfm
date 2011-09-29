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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 352
    Top = 179
    Width = 37
    Height = 13
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
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
  object btnEdit: TButton
    Left = 16
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 7
    OnClick = btnEditClick
  end
  object btnCancel: TButton
    Left = 112
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = btnCancelClick
  end
  object btnSave: TButton
    Left = 208
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 9
    OnClick = btnSaveClick
  end
  object Button2: TButton
    Left = 384
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Validate'
    TabOrder = 10
    OnClick = Button2Click
  end
  object DateTimePicker1: TDateTimePicker
    Left = 352
    Top = 152
    Width = 121
    Height = 21
    Date = 40735.997751724540000000
    Time = 40735.997751724540000000
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
  end
  object BindingGroup1: TBindingGroup
    Left = 200
    Top = 152
    Bindings = <
      item
        Target = ComboBox1
        TargetPropertyName = 'Text'
        Source = Owner
        SourcePropertyName = 'Font.Size'
      end
      item
        Target = CheckBox1
        TargetPropertyName = 'Checked'
        Source = ColorBox1
        SourcePropertyName = 'Enabled'
      end
      item
        Target = ColorBox1
        TargetPropertyName = 'Selected'
        NotifyOnSourceUpdated = True
        Source = Owner
        SourcePropertyName = 'Color'
      end
      item
        Target = Edit2
        TargetPropertyName = 'Text'
        Source = Edit1
        SourcePropertyName = 'Text'
      end
      item
        BindingMode = bmOneWay
        Target = btnCancel
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end
      item
        BindingMode = bmOneWay
        Target = btnSave
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end
      item
        BindingMode = bmOneWay
        Target = btnEdit
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end
      item
        BindingMode = bmOneWay
        Target = ColorBox1
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end
      item
        BindingMode = bmOneWay
        Target = ComboBox1
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end
      item
        BindingMode = bmOneWay
        Target = Edit3
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end
      item
        BindingMode = bmOneWay
        Target = CheckBox1
        TargetPropertyName = 'Enabled'
        Source = BindingGroup1
        SourcePropertyName = 'Editing'
      end>
  end
end
