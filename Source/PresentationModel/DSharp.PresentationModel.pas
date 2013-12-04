unit DSharp.PresentationModel;

interface

{$TYPEINFO ON}

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
  DSharp.Bindings,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.Core.DependencyProperty,
  DSharp.Core.Validations,
  DSharp.PresentationModel.ActionExecutionContext,
  DSharp.PresentationModel.ActivateIntf,
  DSharp.PresentationModel.ActivationEventArgs,
  DSharp.PresentationModel.ActivationEventArgsIntf,
  DSharp.PresentationModel.ActivationProcessedEventArgs,
  DSharp.PresentationModel.ActivationProcessedEventArgsIntf,
  DSharp.PresentationModel.ChildIntf,
  DSharp.PresentationModel.CloseIntf,
  DSharp.PresentationModel.ConductorIntf,
  DSharp.PresentationModel.Coroutine,
  DSharp.PresentationModel.DeactivateIntf,
  DSharp.PresentationModel.DeactivationEventArgs,
  DSharp.PresentationModel.DeactivationEventArgsIntf,
  DSharp.PresentationModel.EventAggregatorIntf,
  DSharp.PresentationModel.Execute,
  DSharp.PresentationModel.GuardCloseIntf,
  DSharp.PresentationModel.IHaveDisplayNameIntf,
  DSharp.PresentationModel.IHaveSubjectIntf,
  DSharp.PresentationModel.IoC,
  DSharp.PresentationModel.ResultBase,
  DSharp.PresentationModel.ResultIntf,
  DSharp.PresentationModel.ResultCompletionEventArgs,
  DSharp.PresentationModel.ResultCompletionEventArgsIntf,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.ScreenIntf,
  DSharp.PresentationModel.ViewAttachedEventArgs,
  DSharp.PresentationModel.ViewAttachedEventArgsIntf,
  DSharp.PresentationModel.ViewAwareIntf,
  DSharp.PresentationModel.ViewSettings,
  DSharp.PresentationModel.ViewModelBase;

const
  {$REGION 'Dialog consts'}
  {$IF COMPILERVERSION > 22}
  mtWarning = System.UITypes.TMsgDlgType.mtWarning;
  mtError = System.UITypes.TMsgDlgType.mtError;
  mtInformation = System.UITypes.TMsgDlgType.mtInformation;
  mtConfirmation = System.UITypes.TMsgDlgType.mtConfirmation;
  mtCustom = System.UITypes.TMsgDlgType.mtCustom;

  mbYes = System.UITypes.TMsgDlgBtn.mbYes;
  mbNo = System.UITypes.TMsgDlgBtn.mbNo;
  mbOK = System.UITypes.TMsgDlgBtn.mbOK;
  mbCancel = System.UITypes.TMsgDlgBtn.mbCancel;
  mbAbort = System.UITypes.TMsgDlgBtn.mbAbort;
  mbRetry = System.UITypes.TMsgDlgBtn.mbRetry;
  mbIgnore = System.UITypes.TMsgDlgBtn.mbIgnore;
  mbAll = System.UITypes.TMsgDlgBtn.mbAll;
  mbNoToAll = System.UITypes.TMsgDlgBtn.mbNoToAll;
  mbYesToAll = System.UITypes.TMsgDlgBtn.mbYesToAll;
  mbHelp = System.UITypes.TMsgDlgBtn.mbHelp;
  mbClose = System.UITypes.TMsgDlgBtn.mbClose;

  mbYesNo = [mbYes, mbNo];
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  mbAbortIgnore = [mbAbort, mbIgnore];

  mrNone = System.UITypes.mrNone;
  mrOk = System.UITypes.mrOk;
  mrCancel = System.UITypes.mrCancel;
  mrAbort = System.UITypes.mrAbort;
  mrRetry = System.UITypes.mrRetry;
  mrIgnore = System.UITypes.mrIgnore;
  mrYes = System.UITypes.mrYes;
  mrNo = System.UITypes.mrNo;
  mrClose = System.UITypes.mrClose;
  mrHelp = System.UITypes.mrHelp;
  mrTryAgain = System.UITypes.mrTryAgain;
  mrContinue = System.UITypes.mrContinue;
  mrAll = System.UITypes.mrAll;
  mrNoToAll = System.UITypes.mrNoToAll;
  mrYesToAll = System.UITypes.mrYesToAll;

  crDefault = System.UITypes.crDefault;
  crNone = System.UITypes.crNone;
  crArrow = System.UITypes.crArrow;
  crCross = System.UITypes.crCross;
  crIBeam = System.UITypes.crIBeam;
  crSize = System.UITypes.crSize;
  crSizeNESW = System.UITypes.crSizeNESW;
  crSizeNS = System.UITypes.crSizeNS;
  crSizeNWSE = System.UITypes.crSizeNWSE;
  crSizeWE = System.UITypes.crSizeWE;
  crUpArrow = System.UITypes.crUpArrow;
  crHourGlass = System.UITypes.crHourGlass;
  crDrag = System.UITypes.crDrag;
  crNoDrop = System.UITypes.crNoDrop;
  crHSplit = System.UITypes.crHSplit;
  crVSplit = System.UITypes.crVSplit;
  crMultiDrag = System.UITypes.crMultiDrag;
  crSQLWait = System.UITypes.crSQLWait;
  crNo = System.UITypes.crNo;
  crAppStart = System.UITypes.crAppStart;
  crHelp = System.UITypes.crHelp;
  crHandPoint = System.UITypes.crHandPoint;
  crSizeAll = System.UITypes.crSizeAll;
  {$ELSE}
  mtWarning = Dialogs.TMsgDlgType.mtWarning;
  mtError = Dialogs.TMsgDlgType.mtError;
  mtInformation = Dialogs.TMsgDlgType.mtInformation;
  mtConfirmation = Dialogs.TMsgDlgType.mtConfirmation;
  mtCustom = Dialogs.TMsgDlgType.mtCustom;

  mbYes = Dialogs.TMsgDlgBtn.mbYes;
  mbNo = Dialogs.TMsgDlgBtn.mbNo;
  mbOK = Dialogs.TMsgDlgBtn.mbOK;
  mbCancel = Dialogs.TMsgDlgBtn.mbCancel;
  mbAbort = Dialogs.TMsgDlgBtn.mbAbort;
  mbRetry = Dialogs.TMsgDlgBtn.mbRetry;
  mbIgnore = Dialogs.TMsgDlgBtn.mbIgnore;
  mbAll = Dialogs.TMsgDlgBtn.mbAll;
  mbNoToAll = Dialogs.mbNoToAll;
  mbYesToAll = Dialogs.mbYesToAll;
  mbHelp = Dialogs.mbHelp;
  mbClose = Dialogs.mbClose;

  mbYesNo = Dialogs.mbYesNo;
  mbYesNoCancel = Dialogs.mbYesNoCancel;
  mbYesAllNoAllCancel = Dialogs.mbYesAllNoAllCancel;
  mbOKCancel = Dialogs.mbOKCancel;
  mbAbortRetryIgnore = Dialogs.mbAbortRetryIgnore;
  mbAbortIgnore = Dialogs.mbAbortIgnore;

  mrNone = Controls.mrNone;
  mrOk = Controls.mrOk;
  mrCancel = Controls.mrCancel;
  mrAbort = Controls.mrAbort;
  mrRetry = Controls.mrRetry;
  mrIgnore = Controls.mrIgnore;
  mrYes = Controls.mrYes;
  mrNo = Controls.mrNo;
  mrAll = Controls.mrAll;
  mrNoToAll = Controls.mrNoToAll;
  mrYesToAll = Controls.mrYesToAll;
  mrClose = Controls.mrClose;

  crDefault = Controls.crDefault;
  crNone = Controls.crNone;
  crArrow = Controls.crArrow;
  crCross = Controls.crCross;
  crIBeam = Controls.crIBeam;
  crSize = Controls.crSize;
  crSizeNESW = Controls.crSizeNESW;
  crSizeNS = Controls.crSizeNS;
  crSizeNWSE = Controls.crSizeNWSE;
  crSizeWE = Controls.crSizeWE;
  crUpArrow = Controls.crUpArrow;
  crHourGlass = Controls.crHourGlass;
  crDrag = Controls.crDrag;
  crNoDrop = Controls.crNoDrop;
  crHSplit = Controls.crHSplit;
  crVSplit = Controls.crVSplit;
  crMultiDrag = Controls.crMultiDrag;
  crSQLWait = Controls.crSQLWait;
  crNo = Controls.crNo;
  crAppStart = Controls.crAppStart;
  crHelp = Controls.crHelp;
  crHandPoint = Controls.crHandPoint;
  crSizeAll = Controls.crSizeAll;
  {$IFEND}
  {$ENDREGION 'Dialog consts'}

type
  {$REGION 'Dialog types'}
  {$IF COMPILERVERSION > 22}
  TMsgDlgType = System.UITypes.TMsgDlgType;
  TMsgDlgBtn = System.UITypes.TMsgDlgType;
  TMsgDlgButtons = System.UITypes.TMsgDlgButtons;
  {$ELSE}
  TMsgDlgType = Dialogs.TMsgDlgType;
  TMsgDlgBtn = Dialogs.TMsgDlgBtn;
  TMsgDlgButtons = Dialogs.TMsgDlgButtons;
  {$IFEND}
  {$ENDREGION 'Dialog types'}
  {$IF CompilerVersion > 22}
  TCloseAction = UITypes.TCloseAction;
  TModalResult = UITypes.TModalResult;
  {$ELSE}
  TCloseAction = Forms.TCloseAction;
  TModalResult = Controls.TModalResult;
  {$IFEND}
  TComponent = Classes.TComponent;

  TValue = Rtti.TValue;

  // {$REGION 'Redefined types from DSharp.Bindings'}
  // BindingAttribute = DSharp.Bindings.BindingAttribute;
  // {$ENDREGION}

  {$REGION 'Redefined types from DSharp.ComponentModel.Composition'}
  AbstractAttribute = DSharp.ComponentModel.Composition.AbstractAttribute;
  ExportAttribute = DSharp.ComponentModel.Composition.ExportAttribute;
  ExportMetaDataAttribute = DSharp.ComponentModel.Composition.
    ExportMetaDataAttribute;
  ImportAttribute = DSharp.ComponentModel.Composition.ImportAttribute;
  ImportBase = DSharp.ComponentModel.Composition.ImportBase;
  ImportLazyAttribute = DSharp.ComponentModel.Composition.ImportLazyAttribute;
  ImportManyAttribute = DSharp.ComponentModel.Composition.ImportManyAttribute;
  ImportingConstructorAttribute = DSharp.ComponentModel.Composition.
    ImportingConstructorAttribute;
  InheritedExportAttribute = DSharp.ComponentModel.Composition.
    InheritedExportAttribute;
  PartCreationPolicyAttribute = DSharp.ComponentModel.Composition.
    PartCreationPolicyAttribute;
  TCreationPolicy = DSharp.ComponentModel.Composition.TCreationPolicy;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.Core.DependencyProperty'}
  TFrameworkPropertyMetadataOption = DSharp.Core.DependencyProperty.
    TFrameworkPropertyMetadataOption;
  IDependencyPropertyChangedEventArgs = DSharp.Core.DependencyProperty.
    IDependencyPropertyChangedEventArgs;
  TDependencyPropertyChangedEventArgs = DSharp.Core.DependencyProperty.
    TDependencyPropertyChangedEventArgs;
  TPropertyChangedEvent = DSharp.Core.DependencyProperty.TPropertyChangedEvent;
  TPropertyMetadata = DSharp.Core.DependencyProperty.TPropertyMetadata;
  TDependencyProperty = DSharp.Core.DependencyProperty.TDependencyProperty;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ActivateIntf'}
  IActivate = DSharp.PresentationModel.ActivateIntf.IActivate;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ActivationEventArgs'}
  TActivationEventArgs = DSharp.PresentationModel.ActivationEventArgs.
    TActivationEventArgs;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ActivationEventArgsIntf'}
  IActivationEventArgs = DSharp.PresentationModel.ActivationEventArgsIntf.
    IActivationEventArgs;
  TActivationEvent = DSharp.PresentationModel.ActivationEventArgsIntf.
    TActivationEvent;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ActivationProcessedEventArgs'}
  TActivationProcessedEventArgs = DSharp.PresentationModel.
    ActivationProcessedEventArgs.TActivationProcessedEventArgs;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ActivationProcessedEventArgsIntf'}
  IActivationProcessedEventArgs = DSharp.PresentationModel.
    ActivationProcessedEventArgsIntf.IActivationProcessedEventArgs;
  TActivationProcessedEvent = DSharp.PresentationModel.
    ActivationProcessedEventArgsIntf.TActivationProcessedEvent;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ActionExecutionContext'}
  IActionExecutionContext = DSharp.PresentationModel.ActionExecutionContext.
    IActionExecutionContext;
  TActionExecutionContext = DSharp.PresentationModel.ActionExecutionContext.
    TActionExecutionContext;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ChildIntf'}
  IChild = DSharp.PresentationModel.ChildIntf.IChild;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.CloseIntf'}
  IClose = DSharp.PresentationModel.CloseIntf.IClose;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ConductorIntf'}
  IParent = DSharp.PresentationModel.ConductorIntf.IParent;
  IHaveActiveItem = DSharp.PresentationModel.ConductorIntf.IHaveActiveItem;
  IConductor = DSharp.PresentationModel.ConductorIntf.IConductor;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.Coroutine'}
  TCoroutine = DSharp.PresentationModel.Coroutine.TCoroutine;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.DeactivateIntf'}
  IDeactivate = DSharp.PresentationModel.DeactivateIntf.IDeactivate;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.DeactivationEventArgs'}
  TDeactivationEventArgs = DSharp.PresentationModel.DeactivationEventArgs.
    TDeactivationEventArgs;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.DeactivationEventArgsIntf'}
  IDeactivationEventArgs = DSharp.PresentationModel.DeactivationEventArgsIntf.
    IDeactivationEventArgs;
  TDeactivationEvent = DSharp.PresentationModel.DeactivationEventArgsIntf.
    TDeactivationEvent;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.EventAggregatorIntf'}
  IHandle = DSharp.PresentationModel.EventAggregatorIntf.IHandle;
  IEventAggregator = DSharp.PresentationModel.EventAggregatorIntf.
    IEventAggregator;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.Execute'}
  Execute = DSharp.PresentationModel.Execute.Execute;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.GuardCloseIntf'}
  IGuardClose = DSharp.PresentationModel.GuardCloseIntf.IGuardClose;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.IHaveDisplayNameIntf'}
  IHaveDisplayName = DSharp.PresentationModel.IHaveDisplayNameIntf.
    IHaveDisplayName;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.IHaveSubjectIntf'}
  IHaveSubject = DSharp.PresentationModel.IHaveSubjectIntf.IHaveSubject;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.IoC'}
  IoC = DSharp.PresentationModel.IoC.IoC;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ResultCompletionEventArgs'}
  TResultCompletionEventArgs = DSharp.PresentationModel.
    ResultCompletionEventArgs.TResultCompletionEventArgs;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ResultCompletionEventArgsIntf'}
  IResultCompletionEventArgs = DSharp.PresentationModel.
    ResultCompletionEventArgsIntf.IResultCompletionEventArgs;
  TResultCompletionEvent = DSharp.PresentationModel.
    ResultCompletionEventArgsIntf.TResultCompletionEvent;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ResultBase'}
  TResultBase = DSharp.PresentationModel.ResultBase.TResultBase;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ResultIntf'}
  IResult = DSharp.PresentationModel.ResultIntf.IResult;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ScreenIntf'}
  IScreen = DSharp.PresentationModel.ScreenIntf.IScreen;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.Screen'}
  TScreen = DSharp.PresentationModel.Screen.TScreen;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ViewAttachedEventArgsIntf'}
  IViewAttachedEventArgs = DSharp.PresentationModel.ViewAttachedEventArgsIntf.
    IViewAttachedEventArgs;
  TViewAttachedEvent = DSharp.PresentationModel.ViewAttachedEventArgsIntf.
    TViewAttachedEvent;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ViewAttachedEventArgs'}
  TViewAttachedEventArgs = DSharp.PresentationModel.ViewAttachedEventArgs.
    TViewAttachedEventArgs;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ViewAwareIntf'}
  IViewAware = DSharp.PresentationModel.ViewAwareIntf.IViewAware;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ViewModelBase'}
  TValidatingScreen = DSharp.PresentationModel.ViewModelBase.TValidatingScreen;
  TViewModel = DSharp.PresentationModel.ViewModelBase.TViewModel;
  {$ENDREGION}
  {$REGION 'Redefined types from DSharp.PresentationModel.ViewSettings'}
  IViewSettings = DSharp.PresentationModel.ViewSettings.IViewSettings;
  TViewSettings = DSharp.PresentationModel.ViewSettings.TViewSettings;
  {$ENDREGION}
  {$REGION 'Attributes'}

  ContentPropertyAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(Name: string);
    property Name: string read FName;
  end;

  BindingAttribute = class(TCustomAttribute)
  private
    FTargetPropertyName: string;
    FBindingString: string;
  public
    constructor Create(const TargetPropertyName: string;
      BindingString: string); overload;
    property TargetPropertyName: string read FTargetPropertyName;
    property BindingString: string read FBindingString;
  end;
  {$ENDREGION 'Attributes'}
  {$REGION 'IWindowManager'}

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
  {$ENDREGION 'IWindowManager'}
  {$REGION 'IDefinesBindings'}

  ///	<summary>
  ///	  TODO
  ///	</summary>

  IDefinesBindings = interface
    ['{9249EEDC-A4B1-491B-9220-9E90ED353A13}']
    procedure DefineBindings;
  end;
  {$ENDREGION 'IDefinesBindings'}

implementation

{ ContentPropertyAttribute }

constructor ContentPropertyAttribute.Create(Name: string);
begin
  FName := Name;
end;

{ BindingAttribute }

constructor BindingAttribute.Create(const TargetPropertyName: string;
  BindingString: string);
begin
  FTargetPropertyName := TargetPropertyName;
  FBindingString := BindingString;
end;

end.
