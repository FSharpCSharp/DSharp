unit Sample1.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  System.Bindings.Controls,
  Sample1.Settings;

type
  TMainForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    ColorBox1: TColorBox;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Button1: TButton;
    Edit3: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    FSettings: TSettings;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Sample1.ValueConverters,
  System.Bindings;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FSettings.Caption := DateTimeToStr(Now);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Create;
  FSettings.Caption := 'Binding demo';
  FSettings.Color := clGray;
  TBinding.Create(FSettings, 'Caption', MainForm, 'Caption');
  TBinding.Create(FSettings, 'Color', MainForm, 'Color');

  TBinding.Create(FSettings, 'Caption', Edit3, 'Text');

  TBinding.Create(Edit1, 'Text', Edit2, 'Text');
  TBinding.Create(Self, 'Color', ColorBox1, 'Selected', bmOneWayToSource);
  TBinding.Create(ColorBox1, 'Enabled', CheckBox1, 'Checked', bmOneWayToSource);
  TBinding.Create(Self.Font, 'Size', ComboBox1, 'Text', bmTwoWay, TIntToStr.Create);
end;

end.
