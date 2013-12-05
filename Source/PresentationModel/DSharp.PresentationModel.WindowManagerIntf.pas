unit DSharp.PresentationModel.WindowManagerIntf;

interface

uses
  Classes,
  Rtti,
  SysUtils,
  {$IF CompilerVersion > 22}
  UITypes,
  {$ELSE}
  Controls,
  Dialogs,
  Forms,
  {$IFEND}
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.ViewSettings;

type

  ///	<summary>
  ///	  A service that manages windows.
  ///	</summary>
  [InheritedExport]
  IWindowManager = interface(IInvokable)
    ['{D0DDEEEF-6148-4410-9304-AA4F8F69E407}']

    function InputBox(const ACaption, APrompt, ADefault: string;
      AShowPasswordChar: Boolean): string;
    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: LongInt = 0): Integer;
    procedure ShowMessage(const Msg: string);

    ///	<summary>
    ///	  Shows a modal dialog for the specified model.
    ///	</summary>
    ///	<param name="rootModel">
    ///	  The root model.
    ///	</param>
    ///	<param name="context">
    ///	  The context.
    ///	</param>
    ///	<param name="settings">
    ///	  The optional dialog settings.
    ///	</param>
    ///	<returns>
    ///	  The dialog result.
    ///	</returns>
    function ShowDialog(RootModel: IInterface): TModalResult; overload;
    function ShowDialog(RootModel: TObject): TModalResult; overload;
    function ShowDialog(RootModel: IInterface; Context: TValue;
      Settings: IViewSettings = nil): TModalResult; overload;
    function ShowDialog(RootModel: TObject; Context: TValue;
      Settings: IViewSettings = nil): TModalResult; overload;

    ///	<summary>
    ///	  Shows a non-modal window for the specified model.
    ///	</summary>
    ///	<param name="rootModel">
    ///	  The root model.
    ///	</param>
    ///	<param name="context">
    ///	  The context.
    ///	</param>
    ///	<param name="settings">
    ///	  The optional window settings.
    ///	</param>
    procedure ShowWindow(RootModel: IInterface); overload;
    procedure ShowWindow(RootModel: TObject); overload;
    procedure ShowWindow(RootModel: IInterface; Context: TValue;
      Settings: IViewSettings = nil); overload;
    procedure ShowWindow(RootModel: TObject; Context: TValue;
      Settings: IViewSettings = nil); overload;
  end;

implementation

end.
