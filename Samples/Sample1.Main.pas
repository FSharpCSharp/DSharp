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
    Button2: TButton;
    DateTimePicker1: TDateTimePicker;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    FSettings: TSettings;
    FColor: TColor;
    FCaption: string;
    FEditing: Boolean;
    FFontSize: Integer;
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
  DSharp.Core.DataConversion,
  DSharp.Core.Validations;

type
  TNegation = class(TValueConverter)
  public
    function Convert(Value: TValue): TValue; override;
    function ConvertBack(Value: TValue): TValue; override;
  end;

  TFutureDateRule = class(TValidationRule)
  public
    function Validate(AValue: TValue): IValidationResult; override;
  end;

procedure TMainForm.BeginEdit;
begin
  if not FEditing then
  begin
    FCaption := Caption;
    FColor := Color;
    FEditing := True;
    FFontSize := Font.Size;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FSettings.Caption := DateTimeToStr(Now);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if not BindingGroup1.Validate then
    MessageDlg(BindingGroup1.ValidationErrors[0].ErrorContent, mtError, [mbOk], 0);
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
    Font.Size := FFontSize;
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
var
  LBinding: TBinding;
  LRule: IValidationRule;
begin
  FSettings := TSettings.Create;
  FSettings.Caption := 'Binding demo';
  FSettings.Color := clBtnFace;
  FSettings.Date := Now();
  TBinding.Create(FSettings, 'Caption', MainForm, 'Caption');
  TBinding.Create(FSettings, 'Color', MainForm, 'Color');
  TBinding.Create(FSettings, 'Caption', Edit3, 'Text');

  // add a validation on this binding and bind a label on its validationerrors
  LBinding := TBinding.Create(FSettings, 'Date', DateTimePicker1, 'Date');
  LRule := TFutureDateRule.Create;
  LBinding.ValidationRules.Add(LRule);
  TBinding.Create(LBinding, 'ValidationErrors[0].ErrorContent', Label1, 'Caption');
//  TBinding.Create(LBinding, 'ValidationErrors[0].ErrorContent', DateTimePicker1, 'Hint');

//  BindingGroup1.ValidationRules.Add(LRule);

  BindingGroup1.GetBindingForTarget(btnEdit).Converter := TNegation.Create();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
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

{ TFutureDateRule }

function TFutureDateRule.Validate(AValue: TValue): IValidationResult;
begin
  if not AValue.IsType<TDate> then
  begin
    Exit(TValidationResult.Create(False, 'Value is not a valid date'));
  end;

  if AValue.AsType<TDate> <= Now then
  begin
    Exit(TValidationResult.Create(False, 'Date must be in the future'));
  end;

  if AValue.AsType<TDate> > IncMonth(Now) then
  begin
    Exit(TValidationResult.Create(False, 'Date must be within one month from today'));
  end;

  Result := TValidationResult.ValidResult();
end;

end.
