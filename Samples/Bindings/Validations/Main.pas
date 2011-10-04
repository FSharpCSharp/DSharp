unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Grids, DSharp.Bindings.VCLControls,
  DSharp.Bindings, pngimage,
  // for the IDataErrorInfo interface we are using in our model class
  DSharp.Core.Validations;

type
  // our data object we are binding to
  // TComponent so we dont have to free it, can also be TObject derivate)

  // we will do a simple view to model binding in this example
  // purpose is to show the validations so we keep it all in this unit
  TModel = class(TComponent, IDataErrorInfo)
  private
    FDate: TDate;
    FTime: TTime;
    procedure SetDate(const Value: TDate);
  protected
    function GetError: string;
    function GetItem(const Name: string): string;
  public
    property Date: TDate read FDate write SetDate;
    property Time: TTime read FTime write FTime;
  end;

  TMainForm = class(TForm)
    DateTimePicker1: TDateTimePicker;
    BindingGroup1: TBindingGroup;
    Label1: TLabel;
    Image1: TImage;
    Button1: TButton;
    DateTimePicker2: TDateTimePicker;
    Image2: TImage;
    DateTimePicker3: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FModel: TModel;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  DateUtils,
  DSharp.Bindings.Notifications,
  DSharp.Core.Logging,
  DSharp.Core.Logging.Console,
// Uncomment if you want to use any of the following logging tools
//  DSharp.Core.Logging.CodeSite,
//  DSharp.Core.Logging.SmartInspect,

  Rtti;

type
  TFutureDateRule = class(TValidationRule)
  public
    function Validate(const Value: TValue): IValidationResult; override;
  end;

  TDelegateRule = class(TValidationRule)
  private
    FDelegate: TFunc<TValue, IValidationResult>;
  public
    constructor Create(ADelegate: TFunc<TValue, IValidationResult>); reintroduce;
    function Validate(const Value: TValue): IValidationResult; override;
  end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  // You can update in both directions explicitely -
  // this clears all possible validation errors on that binding
  BindingGroup1.Bindings[0].UpdateSource;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  binding: TBinding;
begin
  // create our data class and bind the DateTimePicker to it
  FModel := TModel.Create(Self);
  FModel.Date := Date() + 1;
  FModel.Time := 0.5;
  binding := TBinding.Create(FModel, 'Date', DateTimePicker1, 'Date');
  binding.BindingGroup := BindingGroup1;

  // add a validationrule to the binding
  binding.ValidationRules.Add(TFutureDateRule.Create);

  // set up a binding to a possible validation error
  TBinding.Create(binding, 'ValidationErrors[0].ErrorContent', Label1, 'Caption');

  // another option would be to use are little more complex setup

  // the first binding makes use of the default valueconverter
  // it converts 0 to false and any other number to true
  TBinding.Create(binding, 'ValidationErrors.Count', Image1, 'Visible');
  TBinding.Create(binding, 'ValidationErrors[0].ErrorContent', Image1, 'Hint');

  // if the validation fails the value is not transfered to the source of the binding
  // so in that case the control holds an invalid value
  // if you are using error templating as shown above that might be ok
  // however in some situations you might want to set the control value back to a valid state
  // there is no built-in mechanic to provide this behaviour because as long as a binding
  // is about to get updated it does not accept any other incoming changes
  // The following code shows one possible solution to that problem
  // it would also be possible to define attributes on your properties that get bound
  // and use them in the bindings to set default values and to validate them

  binding := TBinding.Create(FModel, 'Time', DateTimePicker2, 'Time');
  // we can enter some invalid value and then exit the control
  // when that happens the validationrule gets triggered and resets it
  binding.SourceUpdateTrigger := utLostFocus;
  binding.ValidationRules.Add(TDelegateRule.Create(
    function(Value: TValue): IValidationResult
    begin
      if not TimeInRange(Value.AsType<TTime>, 0.25, 0.75)  then
      begin
        Result := TValidationResult.Create(False, 'Time must be between 6:00 and 18:00');
        binding.UpdateTarget;
      end
      else
      begin
        Result := TValidationResult.ValidResult;
      end;
    end));

  // another possibility is to implement IDataErrorInfo in the model
  // (or actually view model if you are using the MVVM pattern)
  // and set the ValidatesOnDataErrors property on the binding to true
  binding := TBinding.Create(FModel, 'Date', DateTimePicker3, 'Date');
  binding.ValidatesOnDataErrors := True;
  // this also shows how to set up the binding to prevent a focus change
  // when the value is not valid
  // this only works when SourceUpdateTrigger is set to utLostFocus
  binding.PreventFocusChange := True;
  binding.SourceUpdateTrigger := utLostFocus;
  TBinding.Create(binding, 'ValidationErrors.Count', Image2, 'Visible');
  TBinding.Create(binding, 'ValidationErrors[0].ErrorContent', Image2, 'Hint');

  // that was only a small part of showing the power of DSharp bindings validations

  // if you are using the DWS integration you can create even more powerful
  // expressions to build custom error messages or execute some other things
  // without actually writing compiled delphi code
end;

{ TModel }

function TModel.GetError: string;
begin
  Result := 'Some error occured';
end;

function TModel.GetItem(const Name: string): string;
begin
  Result := '';

  if (Name = 'Date') and (FDate <= Now()) then
  begin
    Result := 'Date must be in the future';
  end;
end;

procedure TModel.SetDate(const Value: TDate);
begin
  FDate := Value;

  // Show logging so you can see when the value is actually updated
  // DateTimePicker however has a kinda weird behaviour when you look at it first
  // when you open the calendar it will trigger twice (when you select a date and when you close it)
  Logging.LogValue<TDate>('Date', Value);
end;

{ TFutureDateRule }

function TFutureDateRule.Validate(const Value: TValue): IValidationResult;
begin
  // invalid if date is not in the future
  if Value.AsType<TDate> <= Now() then
  begin
    Result := TValidationResult.Create(False, 'Date must be in the future');
  end
  else
  begin
    // make sure to return a valid result if everything is ok
    Result := TValidationResult.ValidResult;
  end;
end;

{ TDelegateRule }

constructor TDelegateRule.Create(ADelegate: TFunc<TValue, IValidationResult>);
begin
  FDelegate := ADelegate;
end;

function TDelegateRule.Validate(const Value: TValue): IValidationResult;
begin
  Result := FDelegate(Value);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
