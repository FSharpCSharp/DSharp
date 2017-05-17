object ContactDetailsView: TContactDetailsView
  Left = 0
  Top = 0
  Width = 497
  Height = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object lblTitle: TLabel
    Left = 16
    Top = 24
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object lblLastname: TLabel
    Left = 176
    Top = 24
    Width = 46
    Height = 13
    Caption = 'Lastname'
  end
  object lblFirstname: TLabel
    Left = 336
    Top = 24
    Width = 47
    Height = 13
    Caption = 'Firstname'
  end
  object lblPhone: TLabel
    Left = 16
    Top = 80
    Width = 30
    Height = 13
    Caption = 'Phone'
  end
  object lblFax: TLabel
    Left = 176
    Top = 80
    Width = 18
    Height = 13
    Caption = 'Fax'
  end
  object lblEmail: TLabel
    Left = 336
    Top = 80
    Width = 24
    Height = 13
    Caption = 'Email'
  end
  object lblAddress: TLabel
    Left = 16
    Top = 136
    Width = 39
    Height = 13
    Caption = 'Address'
  end
  object lblZipcode: TLabel
    Left = 16
    Top = 192
    Width = 37
    Height = 13
    Caption = 'Zipcode'
  end
  object lblCity: TLabel
    Left = 176
    Top = 192
    Width = 19
    Height = 13
    Caption = 'City'
  end
  object lblCountry: TLabel
    Left = 336
    Top = 192
    Width = 39
    Height = 13
    Caption = 'Country'
  end
  object Contact_Title: TEdit
    Left = 16
    Top = 40
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object Contact_Lastname: TEdit
    Left = 176
    Top = 40
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object Save: TButton
    Left = 248
    Top = 320
    Width = 112
    Height = 25
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 11
  end
  object Contact_Firstname: TEdit
    Left = 336
    Top = 40
    Width = 145
    Height = 21
    TabOrder = 2
  end
  object Contact_Phone: TEdit
    Left = 16
    Top = 96
    Width = 145
    Height = 21
    TabOrder = 3
  end
  object Contact_Fax: TEdit
    Left = 176
    Top = 96
    Width = 145
    Height = 21
    TabOrder = 4
  end
  object Contact_Email: TEdit
    Left = 336
    Top = 96
    Width = 145
    Height = 21
    TabOrder = 5
  end
  object Contact_Address: TEdit
    Left = 16
    Top = 152
    Width = 465
    Height = 21
    TabOrder = 6
  end
  object Contact_Zipcode: TEdit
    Left = 16
    Top = 208
    Width = 145
    Height = 21
    TabOrder = 7
  end
  object Contact_City: TEdit
    Left = 176
    Top = 208
    Width = 145
    Height = 21
    TabOrder = 8
  end
  object Contact_Country: TEdit
    Left = 336
    Top = 208
    Width = 145
    Height = 21
    TabOrder = 9
  end
  object Cancel: TButton
    Left = 376
    Top = 320
    Width = 105
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object Contact_PreferedContact: TRadioGroup
    Left = 16
    Top = 248
    Width = 305
    Height = 41
    Caption = 'Preferred contact methd:'
    Columns = 2
    Items.Strings = (
      'Phone'
      'Email')
    TabOrder = 10
  end
  object BindingGroup: TBindingGroup
    Left = 56
    Top = 296
    Bindings = <>
  end
end
