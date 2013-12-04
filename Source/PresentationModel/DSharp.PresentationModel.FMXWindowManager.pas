unit DSharp.PresentationModel.FMXWindowManager;

interface

uses
  FMX.Forms,
  UITypes,
  FMX.Types,
  DSharp.PresentationModel.FMXConventionManager,
  Generics.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel;

type
  ///	<summary>
  ///	  A service that manages windows.
  ///	</summary>

  [PartCreationPolicy(cpShared)]
  TWindowManager = class(TInterfacedObject, IWindowManager)
  protected
    FRunning: Boolean;
    procedure ApplySettings(Target: TObject; Settings: IViewSettings);
    function CreateWindow(ARootModel: TObject; AIsDialog: Boolean;
      AContext: TValue; Settings: IViewSettings): TForm;
    function EnsureWindow(AModel: TObject; AView: TComponent;
      AIsDialog: Boolean): TForm;
    function InferOwnerOf(AWindow: TForm): TComponent;
  public
    function InputBox(const ACaption, APrompt, ADefault: string;
      AShowPasswordChar: Boolean): string;
    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: LongInt = 0): Integer;
    procedure ShowMessage(const Msg: string);

    ///	<summary>
    ///	  Shows a modal dialog for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    ///	<returns>
    ///	  The dialog result.
    ///	</returns>
    function ShowDialog(RootModel: IInterface): TModalResult; overload;

    ///	<summary>
    ///	  Shows a modal dialog for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    ///	<param name="Context">
    ///	  The context.
    ///	</param>
    ///	<param name="Settings">
    ///	  The optional window settings.
    ///	</param>
    ///	<returns>
    ///	  The dialog result.
    ///	</returns>
    function ShowDialog(RootModel: IInterface; Context: TValue;
      Settings: IViewSettings = nil): TModalResult; overload;

    ///	<summary>
    ///	  Shows a modal dialog for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    ///	<returns>
    ///	  The dialog result.
    ///	</returns>
    function ShowDialog(RootModel: TObject): TModalResult; overload;

    ///	<summary>
    ///	  Shows a modal dialog for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    ///	<param name="Context">
    ///	  The context.
    ///	</param>
    ///	<param name="Settings">
    ///	  The optional window settings.
    ///	</param>
    ///	<returns>
    ///	  The dialog result.
    ///	</returns>
    function ShowDialog(RootModel: TObject; Context: TValue;
      Settings: IViewSettings = nil): TModalResult; overload;

    ///	<summary>
    ///	  Shows a window for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    procedure ShowWindow(RootModel: IInterface); overload;

    ///	<summary>
    ///	  Shows a window for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    ///	<param name="Context">
    ///	  The context.
    ///	</param>
    ///	<param name="Settings">
    ///	  The optional window settings.
    ///	</param>
    procedure ShowWindow(RootModel: IInterface; Context: TValue;
      Settings: IViewSettings = nil); overload;

    ///	<summary>
    ///	  Shows a window for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    procedure ShowWindow(RootModel: TObject); overload;

    ///	<summary>
    ///	  Shows a window for the specified model.
    ///	</summary>
    ///	<param name="RootModel">
    ///	  The root model.
    ///	</param>
    ///	<param name="Context">
    ///	  The context.
    ///	</param>
    ///	<param name="Settings">
    ///	  The optional window settings.
    ///	</param>
    procedure ShowWindow(RootModel: TObject; Context: TValue;
      Settings: IViewSettings = nil); overload;
  end;

implementation

uses
  Classes,
  FMX.Controls,
  FMX.Dialogs,
  DSharp.Bindings,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Bootstrapper,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.PresentationModel.View,
  DSharp.PresentationModel.FMXChildForm,
  DSharp.PresentationModel.FMXWindowConductor,
  Rtti,
  SysUtils;

procedure TWindowManager.ApplySettings(Target: TObject;
  Settings: IViewSettings);
var
  LPropertyInfo: TRttiProperty;
  LType: TRttiType;
  LPair: TPair<string, TValue>;
begin
  if Assigned(Settings) then
  begin
    for LPair in Settings.Items do
    begin
      LType := GetRttiType(Target.ClassType);
      LPropertyInfo := LType.GetProperty(LPair.Key);
      if Assigned(LPropertyInfo) then
        LPropertyInfo.SetValue(Target, LPair.Value);
    end;
  end;
end;

function TWindowManager.CreateWindow(ARootModel: TObject; AIsDialog: Boolean;
  AContext: TValue; Settings: IViewSettings): TForm;
var
  LBindingGroup: TBindingGroup;
  LHaveDisplayName: IHaveDisplayName;
  LView: TComponent;
  LViewType: TClass;
  LWindow: TForm;
begin
  // Get view type for root model
  LViewType := ViewLocator.LocateTypeForModelType(ARootModel.ClassType, nil,
    AContext);

  if not FRunning and Assigned(LViewType) and LViewType.InheritsFrom(TForm) then
  begin
    // Use Application.CreateForm to create the first form of the application (IoC for the rest)
    Application.CreateForm(TComponentClass(LViewType), LWindow);
    Application.RealCreateForms;
    LView := LWindow;
  end
  else
  begin
    LView := ViewLocator.LocateForModel(ARootModel, nil, AContext)
      as TComponent;
  end;

  LWindow := EnsureWindow(ARootModel, LView, AIsDialog);

  // Exit when view type was not found
  if not Assigned(LViewType) then
    Exit(LWindow);

  ViewModelBinder.Bind(ARootModel, LWindow, AContext);

  if Supports(ARootModel, IHaveDisplayName, LHaveDisplayName) then
  begin
    LBindingGroup := FindBindingGroup(View.GetFirstNonGeneratedView(LWindow)
      as TComponent);
    LBindingGroup.AddBinding(ARootModel, 'DisplayName', LWindow, 'Caption',
      bmOneWay);
    { TODO -o##jwp -cEnhance : Add logging of binding }
  end;

  ApplySettings(LWindow, Settings);

  TWindowConductor.Create(ARootModel, LWindow);

  Result := LWindow;
end;

function TWindowManager.EnsureWindow(AModel: TObject; AView: TComponent;
  AIsDialog: Boolean): TForm;
var
  LOwner: TComponent;
  LForm: TChildForm;
begin
  if AView is TForm then
    Result := AView as TForm
  else
  begin
    if not FRunning then
    begin
      // Use Application.CreateForm to create the first form of the application (IoC for the rest)
      Application.CreateForm(TChildForm, LForm);
      Application.RealCreateForms;
    end
    else
    begin
      LForm := TChildForm.Create(nil);
      LForm.Position := TFormPosition.poOwnerFormCenter;
    end;

    View.IsGeneratedProperty.SetValue(LForm, True);

    LForm.Content := AView as TControl;

    LOwner := InferOwnerOf(LForm);
    if Assigned(LOwner) then
    begin
      LForm.Position := TFormPosition.poOwnerFormCenter;
      // LForm.Owner := LOwner;
    end
    else
    begin
      LForm.Position := TFormPosition.poScreenCenter;
    end;
    Result := LForm;
  end;
end;

function TWindowManager.InferOwnerOf(AWindow: TForm): TComponent;
var
  LActiveWindow: TComponent;
begin
  if not Assigned(Application.MainForm) then
    Exit(nil);

  LActiveWindow := nil; // TODO : How to get Screen.ActiveForm?

  if not Assigned(LActiveWindow) then
    LActiveWindow := Application.MainForm;

  if LActiveWindow = AWindow then
    Result := nil
  else
    Result := LActiveWindow;
end;

function TWindowManager.InputBox(const ACaption, APrompt, ADefault: string;
  AShowPasswordChar: Boolean): string;
begin
  Result := FMX.Dialogs.InputBox(ACaption, APrompt, ADefault);
end;

function TWindowManager.MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer): Integer;
begin
  {$IF COMPILERVERSION < 23}
  Result := FMX.Dialogs.MessageDlg(Msg, Dialogs.TMsgDlgType(DlgType),
    Dialogs.TMsgDlgButtons(Buttons), HelpCtx);
  {$ELSE}
  Result := FMX.Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx);
  {$IFEND}
end;

function TWindowManager.ShowDialog(RootModel: IInterface): TModalResult;
begin
  Result := ShowDialog(RootModel as TObject, nil, nil);
end;

function TWindowManager.ShowDialog(RootModel: IInterface; Context: TValue;
  Settings: IViewSettings): TModalResult;
begin
  Result := ShowDialog(RootModel as TObject, Context, Settings);
end;

function TWindowManager.ShowDialog(RootModel: TObject): TModalResult;
begin
  Result := ShowDialog(RootModel, nil, nil);
end;

function TWindowManager.ShowDialog(RootModel: TObject; Context: TValue;
  Settings: IViewSettings): TModalResult;
var
  LKeepAlive: IInterface;
begin
  // Keep model alive in scope
  Supports(RootModel, IInterface, LKeepAlive);
  Result := CreateWindow(RootModel, True, Context, Settings).ShowModal;
end;

procedure TWindowManager.ShowMessage(const Msg: string);
begin
  FMX.Dialogs.ShowMessage(Msg);
end;

procedure TWindowManager.ShowWindow(RootModel: IInterface);
begin
  ShowWindow(RootModel as TObject, nil, nil);
end;

procedure TWindowManager.ShowWindow(RootModel: IInterface; Context: TValue;
  Settings: IViewSettings);
begin
  ShowWindow(RootModel, Context, Settings);
end;

procedure TWindowManager.ShowWindow(RootModel: TObject);
begin
  ShowWindow(RootModel, nil, nil);
end;

procedure TWindowManager.ShowWindow(RootModel: TObject; Context: TValue;
  Settings: IViewSettings);
var
  LKeepAlive: IInterface;
  LWindow: TForm;
begin
  // Keep model alive in scope
  Supports(RootModel, IInterface, LKeepAlive);

  LWindow := CreateWindow(RootModel, False, Context, Settings);

  if not FRunning then
  begin
    LWindow.Caption := Application.Title;
    FRunning := True;
    Application.Run;
  end
  else
  begin
    LWindow.Show;
  end;
end;

initialization

TWindowManager.ClassName;

end.
