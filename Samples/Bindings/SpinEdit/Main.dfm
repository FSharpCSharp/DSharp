object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DSharp Bindings in VCL - how to do it right! ;)'
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
  DesignSize = (
    554
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTop: TLabel
    Left = 16
    Top = 19
    Width = 22
    Height = 13
    Caption = 'Top:'
    FocusControl = seTop
  end
  object lblLeft: TLabel
    Left = 16
    Top = 47
    Width = 23
    Height = 13
    Caption = 'Left:'
    FocusControl = seLeft
  end
  object lblWidth: TLabel
    Left = 16
    Top = 75
    Width = 32
    Height = 13
    Caption = 'Width:'
    FocusControl = seWidth
  end
  object lblHeight: TLabel
    Left = 16
    Top = 103
    Width = 35
    Height = 13
    Caption = 'Height:'
    FocusControl = seHeight
  end
  object seTop: TSpinEdit
    Left = 80
    Top = 16
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object seLeft: TSpinEdit
    Left = 80
    Top = 44
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object seWidth: TSpinEdit
    Left = 80
    Top = 72
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object seHeight: TSpinEdit
    Left = 80
    Top = 100
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object pnlPicture: TPanel
    Left = 224
    Top = 8
    Width = 322
    Height = 274
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object Image1: TImage
      Left = 112
      Top = 88
      Width = 105
      Height = 105
    end
  end
  object BindingGroup1: TBindingGroup
    Left = 72
    Top = 160
    Bindings = <
      item
        Target = seTop
        TargetPropertyName = 'Value'
        Source = Image1
        SourcePropertyName = 'Top'
      end
      item
        Target = seLeft
        TargetPropertyName = 'Value'
        Source = Image1
        SourcePropertyName = 'Left'
      end
      item
        Target = seWidth
        TargetPropertyName = 'Value'
        Source = Image1
        SourcePropertyName = 'Width'
      end
      item
        Target = seHeight
        TargetPropertyName = 'Value'
        Source = Image1
        SourcePropertyName = 'Height'
      end>
  end
end
