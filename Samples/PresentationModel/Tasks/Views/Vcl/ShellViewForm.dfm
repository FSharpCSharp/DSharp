object ShellView: TShellView
  Left = 0
  Top = 0
  ClientHeight = 546
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 15
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 698
    Height = 546
    Align = alClient
    Caption = 'GridPanel1'
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 500.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = Tasks
        Row = 1
      end
      item
        Column = 1
        Control = Label1
        Row = 0
      end>
    FullRepaint = False
    RowCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 100.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 50.000000000000000000
      end>
    ShowCaption = False
    TabOrder = 0
    object Tasks: TPanel
      Left = 99
      Top = 101
      Width = 500
      Height = 394
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Tasks'
      FullRepaint = False
      ShowCaption = False
      TabOrder = 0
    end
    object Label1: TLabel
      Left = 99
      Top = 1
      Width = 134
      Height = 62
      Align = alClient
      Alignment = taCenter
      Caption = 'tasks'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -53
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
