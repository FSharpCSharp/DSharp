unit AppViewFormFmx;

interface

uses
//  System.SysUtils,
//  System.Types,
//  System.UITypes,
  System.Classes,
//  System.Variants,
  FMX.Types,
//  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
//  FMX.Dialogs,
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
(*
//    [Binding('OnExecute', '{Binding IncrementCountBy2}')]
//    [Binding('Enabled', '{Binding CanIncrementCountBy2}')]
    IncrementByTwo: TAction;
//    [Binding('OnExecute', '{Binding MultiplyCountBy2}')]
//    [Binding('Enabled', '{Binding CanMultiplyCountBy2}')]
    MultipyByTwo: TAction;
    ActionToolBar1: TActionToolBar;
    ActionMainMenuBar1: TActionMainMenuBar;
//    [Binding('Caption', '{Binding IncrementValue}')]
    IncVal: TLabel;
    IncrementCountByIncrementValue: TButton;
*)
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
