unit Sample1.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  System.Bindings.Controls;

type
  TMainForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    ColorBox1: TColorBox;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Rtti,
  System.Bindings;

type
  TIntToStr = class(TInterfacedObject, IValueConverter)
  public
    function Convert(Value: TValue): TValue;
    function ConvertBack(Value: TValue): TValue;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TBinding.Create(Edit1, 'Text', Edit2, 'Text');
  TBinding.Create(Self, 'Color', ColorBox1, 'Selected', bmOneWayToSource);
  TBinding.Create(ColorBox1, 'Enabled', CheckBox1, 'Checked', bmOneWayToSource);
  TBinding.Create(Self.Font, 'Size', ComboBox1, 'Text', bmTwoWay, TIntToStr.Create);
end;

{ TIntToStr }

function TIntToStr.Convert(Value: TValue): TValue;
begin
  Result := TValue.From<string>(Value.ToString());
end;

function TIntToStr.ConvertBack(Value: TValue): TValue;
begin
  Result := TValue.From<Integer>(StrToInt(Value.ToString));
end;

end.
