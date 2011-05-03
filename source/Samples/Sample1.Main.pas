unit Sample1.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sample1.Settings, ExtCtrls, StdCtrls, ComCtrls,
  System.Bindings.Controls.VCL, System.Bindings;

type
  TMainForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    ColorBox1: TColorBox;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Button1: TButton;
    Edit3: TEdit;
    BindingGroup1: TBindingGroup;
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

  ComboBox1.Binding.Source := Self.Font;
end;

end.
