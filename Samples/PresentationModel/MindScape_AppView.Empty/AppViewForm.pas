unit AppViewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  Vcl.ActnMenus, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ComCtrls,
//A  unhide the controls we need.
//B  paste the TActionToolBar/TActionMainMenuBar
//C  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;

type
  TAppView = class(TForm
//E  , IDefinesBindings
  )
    Count: TEdit;
    IncrementCount: TButton;
    DecrementCount: TButton;
    Button1: TButton;
    Button2: TButton;
    ActionManager1: TActionManager;
//C    [Binding('OnExecute', '{Binding IncrementCountBy2}')]
//C    [Binding('Enabled', '{Binding CanIncrementCountBy2}')]
    IncrementByTwo: TAction;
//C    [Binding('OnExecute', '{Binding MultiplyCountBy2}')]
//C    [Binding('Enabled', '{Binding CanMultiplyCountBy2}')]
    MultipyByTwo: TAction;
    IncrementValue: TTrackBar;
//C    [Binding('Caption', '{Binding IncrementValue}')]
    IncVal: TLabel;
    IncrementCountByIncrementValue: TButton;
  public
    // This method will be called by the framework to create bindings
//D    procedure DefineBindings;
  end;

implementation

//uses
//  // You need to include unit dsharp.core.extensions to be able to create bindings in code
//D  DSharp.Core.Extensions;

{$R *.dfm}

//D
//procedure TAppView.DefineBindings;
//begin
//  // Create bindings in code
//  IncrementByTwo.SetBinding('OnExecute', 'IncrementCountBy2');
//  IncrementByTwo.SetBinding('Enabled', 'CanIncrementCountBy2');
//  MultipyByTwo.SetBinding('OnExecute', 'MultiplyCountBy2');
//  MultipyByTwo.SetBinding('Enabled', 'CanMultiplyCountBy2');
//  IncVal.SetBinding('Caption', 'IncrementValue');
//end;

initialization
  TAppView.ClassName;
end.

//B
{
object ActionToolBar1: TActionToolBar
  Left = 0
  Top = 0
  Width = 303
  Height = 23
  ActionManager = ActionManager1
  Align = alNone
  Caption = 'ActionToolBar1'
  Color = clMenuBar
  ColorMap.DisabledFontColor = 7171437
  ColorMap.HighlightColor = clWhite
  ColorMap.BtnSelectedFont = clBlack
  ColorMap.UnusedColor = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  Spacing = 0
  Visible = False
end
object ActionMainMenuBar1: TActionMainMenuBar
  Left = 0
  Top = 23
  Width = 303
  Height = 25
  UseSystemFont = False
  ActionManager = ActionManager1
  Align = alNone
  Caption = 'ActionMainMenuBar1'
  Color = clMenuBar
  ColorMap.DisabledFontColor = 7171437
  ColorMap.HighlightColor = clWhite
  ColorMap.BtnSelectedFont = clBlack
  ColorMap.UnusedColor = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Spacing = 0
end
}
