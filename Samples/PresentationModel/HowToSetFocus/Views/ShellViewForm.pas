unit ShellViewForm;

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
  ExtCtrls,
  System.Actions,
  Vcl.ActnList,
  DSharp.PresentationModel,
  DSharp.Bindings.VCLControls;
// <--- Warning: This line must always be the last one in uses clause!

type
  ///	<summary>
  ///	  View for ShellView
  ///	</summary>
  TShellView = class(TForm)
    // Here we bind IsFocused "dependency property" to the IsFirstNameFocused property on the view model
    [Binding('TFocusExtension.IsFocused', '{Binding IsFirstNameFocused}')]
    FirstName: TEdit;
    // Here we bind IsFocused "dependency property" to the IsLastNameFocused property on the view model
    [Binding('TFocusExtension.IsFocused', '{Binding IsLastNameFocused}')]
    LastName: TEdit;
    ActionList1: TActionList;
    ActionFocusFirstName: TAction;
    ActionFocusLastName: TAction;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
  end;

implementation

{$R *.dfm}

initialization

TShellView.ClassName;

end.
