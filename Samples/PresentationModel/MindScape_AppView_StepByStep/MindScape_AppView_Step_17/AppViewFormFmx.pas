unit AppViewFormFmx;

interface

uses
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Edit,
  System.Actions,
  FMX.ActnList, 
  FMX.Menus,
  DSharp.PresentationModel,
  DSharp.Bindings.FMXControls;

type
  TAppView = class(TForm, IDefinesBindings)
    Count: TEdit;
    Button1: TButton;
    ActionList1: TActionList;
    IncrementByTwo: TAction;
    MultipyByTwo: TAction;
    DecrementCount: TButton;
    IncrementCount: TButton;
    Button2: TButton;
    IncVal: TLabel;
    IncrementValue: TTrackBar;
    IncrementCountByIncrementValue: TButton;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MainMenu1: TMainMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    FrequencyIncrementValue: TTrackBar;
    FloatIncrementValue: TTrackBar;
    [Binding('Value',
      '{Binding Path=IncrementValue, Converter=TIntegerToRoundedFloatConverter}')]
    ConvertedIncrementValue: TTrackBar;
  public
    procedure DefineBindings();
  end;

implementation

uses
  DSharp.PresentationModel.Extensions;

{$R *.fmx}

{ TAppView }

procedure TAppView.DefineBindings();
begin
  IncrementByTwo.SetBinding('OnExecute', 'IncrementCountBy2');
  IncrementByTwo.SetBinding('Enabled', 'CanIncrementCountBy2');
  MultipyByTwo.SetBinding('OnExecute', 'MultiplyCountBy2');
  MultipyByTwo.SetBinding('Enabled', 'CanMultiplyCountBy2');
  IncVal.SetBinding('Text', 'IncrementValue');
  {TODO -o##jwp -cImprovement : Would it be wise to add a constant to each interceptor class indicating the primary Value property name?}
  FrequencyIncrementValue.SetBinding('Value', 'IncrementValue');
end;

initialization
  TAppView.ClassName;
end.
