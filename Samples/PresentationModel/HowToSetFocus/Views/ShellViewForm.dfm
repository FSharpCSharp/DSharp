object ShellView: TShellView
  Left = 0
  Top = 0
  ClientHeight = 699
  ClientWidth = 779
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 88
    Width = 51
    Height = 13
    Caption = 'First Name'
  end
  object Label2: TLabel
    Left = 48
    Top = 171
    Width = 50
    Height = 13
    Caption = 'Last Name'
  end
  object FirstName: TEdit
    Left = 48
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object LastName: TEdit
    Left = 48
    Top = 187
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 192
    Top = 102
    Width = 75
    Height = 25
    Action = ActionFocusFirstName
    TabOrder = 2
  end
  object Button2: TButton
    Left = 192
    Top = 185
    Width = 75
    Height = 25
    Action = ActionFocusLastName
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 432
    Top = 112
    object ActionFocusFirstName: TAction
      Caption = 'Set Focus'
    end
    object ActionFocusLastName: TAction
      Caption = 'Set Focus'
    end
  end
end
