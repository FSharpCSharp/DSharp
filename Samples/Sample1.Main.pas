unit Sample1.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, DSharp.Bindings.VCLControls,
  DSharp.Bindings, Sample1.Settings;

type
  TMainForm = class(TForm, IEditable)
    Edit1: TEdit;
    Edit2: TEdit;
    ColorBox1: TColorBox;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Button1: TButton;
    Edit3: TEdit;
    BindingGroup1: TBindingGroup;
    btnEdit: TButton;
    btnCancel: TButton;
    btnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FSettings: TSettings;
    FColor: TColor;
    FCaption: string;
    FEditing: Boolean;
  public
    { Public-Deklarationen }
    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DSharp.Core.DataConversion;

type
  TNegation = class(TValueConverter)
  public
    function Convert(Value: TValue): TValue; override;
    function ConvertBack(Value: TValue): TValue; override;
  end;

procedure TMainForm.BeginEdit;
begin
  if not FEditing then
  begin
    FCaption := Caption;
    FColor := Color;
    FEditing := True;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FSettings.Caption := DateTimeToStr(Now);
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  BindingGroup1.CancelEdit;
  BindingGroup1.UpdateTargets;
end;

procedure TMainForm.btnEditClick(Sender: TObject);
begin
  BindingGroup1.BeginEdit;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  BindingGroup1.CommitEdit;
end;

procedure TMainForm.CancelEdit;
begin
  if FEditing then
  begin
    Caption := FCaption;
    Color := FColor;
    FEditing := False;
  end;
end;

procedure TMainForm.EndEdit;
begin
  if FEditing then
  begin
    FEditing := False;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Create;
  FSettings.Caption := 'Binding demo';
  FSettings.Color := clGray;
  TBinding.Create(FSettings, 'Caption', MainForm, 'Caption');
  TBinding.Create(FSettings, 'Color', MainForm, 'Color');
  TBinding.Create(FSettings, 'Caption', Edit3, 'Text');

  BindingGroup1.GetBindingForTarget(btnEdit).Converter := TNegation.Create();
end;

{ TNegation }

function TNegation.Convert(Value: TValue): TValue;
begin
  Result := not Value.AsBoolean;
end;

function TNegation.ConvertBack(Value: TValue): TValue;
begin
  Result := not Value.AsBoolean;
end;

end.
