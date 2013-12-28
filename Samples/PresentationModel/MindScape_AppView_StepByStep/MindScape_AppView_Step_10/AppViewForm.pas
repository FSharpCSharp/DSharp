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
  TAppView = class(TForm)
    Count: TEdit;
    IncrementCount: TButton;
    DecrementCount: TButton;
    ActionManager1: TActionManager;
    [Binding('OnExecute', '{Binding IncrementCountBy2}')]
    [Binding('Enabled', '{Binding CanIncrementCountBy2}')]
    IncrementByTwo: TAction;
    [Binding('OnExecute', '{Binding MultiplyCountBy2}')]
    [Binding('Enabled', '{Binding CanMultiplyCountBy2}')]
    MultipyByTwo: TAction;
    Button1: TButton;
    Button2: TButton;
    ActionToolBar1: TActionToolBar;
    ActionMainMenuBar1: TActionMainMenuBar;
  end;

implementation

{$R *.dfm}

initialization
  TAppView.ClassName;
end.
