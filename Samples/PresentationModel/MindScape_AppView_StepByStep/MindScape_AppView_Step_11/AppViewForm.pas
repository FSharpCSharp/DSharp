unit AppViewForm;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  ActnMenus,
  ToolWin,
  ActnMan,
  ActnCtrls,
  Actions,
  ActnList,
  PlatformDefaultStyleActnCtrls,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TAppView = class(TForm, IDefinesBindings)
    Count: TEdit;
    IncrementCount: TButton;
    DecrementCount: TButton;
    ActionManager1: TActionManager;
//    [Binding('OnExecute', '{Binding IncrementCountBy2}')]
//    [Binding('Enabled', '{Binding CanIncrementCountBy2}')]
    IncrementByTwo: TAction;
//    [Binding('OnExecute', '{Binding MultiplyCountBy2}')]
//    [Binding('Enabled', '{Binding CanMultiplyCountBy2}')]
    MultipyByTwo: TAction;
    Button1: TButton;
    Button2: TButton;
    ActionToolBar1: TActionToolBar;
    ActionMainMenuBar1: TActionMainMenuBar;
  public
    procedure DefineBindings;
  end;

implementation

uses
  DSharp.PresentationModel.Extensions;

{$R *.dfm}

{ TAppView }

procedure TAppView.DefineBindings;
begin
  IncrementByTwo.SetBinding('OnExecute', 'IncrementCountBy2');
  IncrementByTwo.SetBinding('Enabled', 'CanIncrementCountBy2');
  MultipyByTwo.SetBinding('OnExecute', 'MultiplyCountBy2');
  MultipyByTwo.SetBinding('Enabled', 'CanMultiplyCountBy2');
end;

initialization
  TAppView.ClassName;
end.
