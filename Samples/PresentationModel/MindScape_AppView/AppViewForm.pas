unit AppViewForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Vcl.ActnMenus,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  System.Actions,
  Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ComCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TAppView = class(TForm, IDefinesBindings)
    Count: TEdit;
    IncrementCount: TButton;
    DecrementCount: TButton;
    Button1: TButton;
    Button2: TButton;
    ActionManager1: TActionManager;
    // [Binding('OnExecute', '{Binding IncrementCountBy2}')]
    // [Binding('Enabled', '{Binding CanIncrementCountBy2}')]
    IncrementByTwo: TAction;
    // [Binding('OnExecute', '{Binding MultiplyCountBy2}')]
    // [Binding('Enabled', '{Binding CanMultiplyCountBy2}')]
    MultipyByTwo: TAction;
    ActionToolBar1: TActionToolBar;
    ActionMainMenuBar1: TActionMainMenuBar;
    IncrementValue: TTrackBar;
    // [Binding('Caption', '{Binding IncrementValue}')]
    IncVal: TLabel;
    IncrementCountByIncrementValue: TButton;
  public
    // This method will be called by the framework to create bindings
    procedure DefineBindings;
  end;

implementation

uses
  // You need to include unit dsharp.core.extensions to be able to create bindings in code
  DSharp.PresentationModel.Extensions;

{$R *.dfm}
{ TAppView }

procedure TAppView.DefineBindings;
begin
  // Create bindings in code
  IncrementByTwo.SetBinding('OnExecute', 'IncrementCountBy2');
  IncrementByTwo.SetBinding('Enabled', 'CanIncrementCountBy2');
  MultipyByTwo.SetBinding('OnExecute', 'MultiplyCountBy2');
  MultipyByTwo.SetBinding('Enabled', 'CanMultiplyCountBy2');
  IncVal.SetBinding('Caption', 'IncrementValue');
end;

initialization

TAppView.ClassName;

end.
