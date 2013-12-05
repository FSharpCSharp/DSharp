unit DSharp.PresentationModel.ViewModelBinder;

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DSharp.Collections,
  DSharp.Logging,
  DSharp.PresentationModel;

type
  ///	<summary>
  ///	  Binds a view to a view model.
  ///	</summary>
  ViewModelBinder = class
  private
  class var
    FApplyConventionsByDefault: Boolean;
    FConventionsAppliedProperty: TDependencyProperty;
    FLog: ILog;
    FBindCustomAction: TFunc<TComponent, TObject, TRttiMethod, Boolean>;
    class function GetLog: ILog; static;
    class property Log: ILog read GetLog;
  public
    class constructor Create;

    ///	<summary>
    ///	  Determines whether a view should have conventions applied to it.
    ///	</summary>
    ///	<param name="view">
    ///	  The view to check.
    ///	</param>
    ///	<returns>
    ///	  Whether or not conventions should be applied to the view.
    ///	</returns>
    class function ShouldApplyConventions(AView: TComponent): Boolean;

    ///	<summary>
    ///	  Binds the specified viewModel to the view.
    ///	</summary>
    ///	<remarks>
    ///	  Passes the the view model, view and creation context (or null for
    ///	  default) to use in applying binding.
    ///	</remarks>
    class procedure Bind(AViewModel: TObject; AView: TComponent;
      AContext: TValue);

    ///	<summary>
    ///	  Creates data bindings on the view's controls based on the provided
    ///	  properties.
    ///	</summary>
    ///	<remarks>
    ///	  Parameters include named Elements to search through and the type of
    ///	  view model to determine conventions for. Returns unmatched elements.
    ///	</remarks>
    class function BindProperties(ANamedElements: IEnumerable<TComponent>;
      AViewModel: TObject; AView: TComponent): IEnumerable<TComponent>;

    ///	<summary>
    ///	  Attaches instances of <see cref="ActionMessage" /> to the view's
    ///	  controls based on the provided methods.
    ///	</summary>
    ///	<remarks>
    ///	  Parameters include the named elements to search through and the type
    ///	  of view model to determine conventions for. Returns unmatched
    ///	  elements.
    ///	</remarks>
    class function BindActions(ANamedElements: IEnumerable<TComponent>;
      AViewModelType: TClass): IEnumerable<TComponent>;

    ///	<summary>
    ///	  Attaches instances of <see cref="ActionMessage" /> to the view's
    ///	  controls based on the provided methods.
    ///	</summary>
    ///	<remarks>
    ///	  Parameters include the named elements to search through and the type
    ///	  of view model to determine conventions for. Returns unmatched
    ///	  elements.
    ///	</remarks>
    class function BindActionsSimplified(ANamedElements
      : IEnumerable<TComponent>; AViewModel: TObject; AView: TComponent)
      : IEnumerable<TComponent>;

    ///	<summary>
    ///	  Applies global conventions
    ///	</summary>
    ///	<remarks>
    ///	  Parameters include the named elements to search through and the type
    ///	  of view model to determine conventions for. Returns unmatched
    ///	  elements.
    ///	</remarks>
    class function ApplyGlobalConventions(ANamedElements
      : IEnumerable<TComponent>; AViewModel: TObject; AView: TComponent)
      : IEnumerable<TComponent>;
    class function FindControlAndRemove(const LUnmatchedElements
      : IList<TComponent>; const LMethod: TRttiMethod): TComponent;

    ///	<summary>
    ///	  Allows the developer to add custom handling of named elements which
    ///	  were not matched by any default conventions.
    ///	</summary>
    class function HandleUnmatchedElements(ANamedElements
      : IEnumerable<TComponent>; AViewModelType: TClass)
      : IEnumerable<TComponent>;

    ///	<summary>
    ///	  Gets or sets a value indicating whether to apply conventions by
    ///	  default.
    ///	</summary>
    ///	<value>
    ///	  <c>True</c> if conventions should be applied by default; otherwise,
    ///	  <c>False</c>.
    ///	</value>
    class property ApplyConventionsByDefault: Boolean
      read FApplyConventionsByDefault write FApplyConventionsByDefault;

    ///	<summary>
    ///	  Indicates whether or not the conventions have already been applied to
    ///	  the view.
    ///	</summary>
    class property ConventionsAppliedProperty: TDependencyProperty
      read FConventionsAppliedProperty;

    class property BindCustomAction
      : TFunc<TComponent, TObject, TRttiMethod, Boolean> read FBindCustomAction
      write FBindCustomAction;
  end;

implementation

uses
  StrUtils,
  Types,
  DSharp.Bindings,
  DSharp.Core.Events,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Action,
  DSharp.PresentationModel.Bind,
  DSharp.PresentationModel.BindingScope,
  DSharp.PresentationModel.ConventionManager,
  DSharp.PresentationModel.ElementConvention,
  DSharp.PresentationModel.View,
  DSharp.PresentationModel.InitializeComponent,
  DSharp.PresentationModel.CoroutineExecutionContext,
  DSharp.PresentationModel.CoroutineExecutionContextIntf;

class constructor ViewModelBinder.Create;
begin
  FApplyConventionsByDefault := True;
  FConventionsAppliedProperty := TDependencyProperty.RegisterAttached
    ('ConventionsApplied', TypeInfo(Boolean), ViewModelBinder, nil);

  FBindCustomAction :=
      function(Action: TComponent; ViewModel: TObject;
      Method: TRttiMethod): Boolean
    begin
      Result := False;
    end;
end;

class function ViewModelBinder.GetLog: ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(ViewModelBinder));
  end;
  Result := FLog;
end;

class function ViewModelBinder.HandleUnmatchedElements(ANamedElements
  : IEnumerable<TComponent>; AViewModelType: TClass): IEnumerable<TComponent>;
begin
end;

class function ViewModelBinder.ShouldApplyConventions
  (AView: TComponent): Boolean;
var
  LOverriden: Integer;
begin
  LOverriden := View.GetApplyConventions(AView);
  if LOverriden = 0 then
    Result := ApplyConventionsByDefault
  else
    Result := LOverriden > 0;
end;

{ ViewModelBinder }

class procedure ViewModelBinder.Bind(AViewModel: TObject; AView: TComponent;
  AContext: TValue);
var
  LElement, LItem: TComponent;
  LViewAware: IViewAware;
  LNamedElements: IEnumerable<TComponent>;
  LViewModelType: TClass;
begin
  Log.LogMessage('Binding %s and %s.', [AView.ToString, AViewModel.ToString]);

  if DSharp.PresentationModel.Bind.Bind.NoContextProperty.GetValue(AView).AsBoolean
  then
    Action.SetTargetWithoutContext(AView, AViewModel)
  else
    Action.SetTarget(AView, AViewModel);

  if Supports(AViewModel, IViewAware, LViewAware) then
  begin
    Log.LogMessage('Attaching %s to %s.',
      [AView.ToString, TValue.From(LViewAware)]);
    LViewAware.AttachView(AView, AContext);
  end;

  if ConventionsAppliedProperty.GetValue(AView).AsBoolean then
    Exit;

  LElement := View.GetFirstNonGeneratedView(AView) as TComponent;
  if LElement = nil then
    Exit;

  if not ShouldApplyConventions(LElement) then
  begin
    Log.LogMessage('Skipping conventions for %s and %s.',
      [LElement.ToString, AViewModel.ToString]);
    Exit;
  end;

  LViewModelType := AViewModel.ClassType;
  LNamedElements := BindingScope.GetNamedElements(LElement);

  for LItem in LNamedElements do
    View.IsLoadedProperty.SetValue(LItem,
      View.IsLoadedProperty.GetValue(LElement));

  LNamedElements := ApplyGlobalConventions(LNamedElements, AViewModel,
    LElement);
  LNamedElements := BindActionsSimplified(LNamedElements, AViewModel, LElement);
  LNamedElements := BindProperties(LNamedElements, AViewModel, LElement);
  HandleUnmatchedElements(LNamedElements, LViewModelType);

  ConventionsAppliedProperty.SetValue(AView, True);

  // Initialize dependency properties on the view
  TInitializeComponent.Initialize(LElement);
end;

class function ViewModelBinder.BindActions(ANamedElements
  : IEnumerable<TComponent>; AViewModelType: TClass): IEnumerable<TComponent>;
var
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LUnmatchedElements: IList<TComponent>;
  LFoundControl: TComponent;
  LViewModelType: TRttiType;
begin
  LUnmatchedElements := ANamedElements.ToList();
  LViewModelType := GetRttiType(AViewModelType);
  LMethods := LViewModelType.GetMethods();

  for LMethod in LMethods do
  begin
    LFoundControl := FindControlAndRemove(LUnmatchedElements, LMethod);
    if LFoundControl = nil then
      Continue;

    // TODO: Insert code for binding

    Log.LogMessage('Action Convention Applied: Action %s on element %s.',
      [LMethod.Describe(), LFoundControl.Describe()]);
    // Message.SetAttach(LFoundControl, message);
  end;

  Result := LUnmatchedElements;
end;

function Trim(const S: string; C: Char): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = C) do
    Inc(I);
  if I > L then
    Exit('');
  while S[L] = C do
    Dec(L);
  Result := Copy(S, I, L - I + 1);
end;

{$REGION 'Documentation'}
/// <summary>
/// Gets a property by name, ignoring case and searching all interfaces.
/// </summary>
/// <param name="type">
/// The type to inspect.
/// </param>
/// <param name="propertyName">
/// The property to search for.
/// </param>
/// <returns>
/// The property or null if not found.
/// </returns>
{$ENDREGION}

function GetPropertyCaseInsensitive(AType: TRttiType; AName: string)
  : TRttiProperty;
begin
  Result := AType.GetProperty(AName);
  // TODO GetInterfaces
  // http://hallvards.blogspot.com/2006/04/getting-list-of-implemented-interfaces.html
end;

function BindNotifyEvent(AControl: TComponent; AEventName: string;
  AViewModel: TObject; AView: TComponent;
  AViewModelMethod: TRttiMethod): Boolean;
var
  LHandlerValue: TValue;
  LNotifyEvent: TNotifyEvent;
  LEvent: TEvent<TNotifyEvent>;
  LEventProperty: TRttiProperty;
  LResult: IResult;
begin
  Result := False;
  LEventProperty := GetRttiType(AControl.ClassInfo).GetProperty(AEventName);
  // Check
  if not Assigned(LEventProperty) then
    Exit;
  if not Assigned(AViewModelMethod.ReturnType) then
    Exit;
  if (AViewModelMethod.ReturnType.Handle <> TypeInfo(IResult)) and
    (AViewModelMethod.ReturnType.Handle <> TypeInfo(IEnumerable<IResult>)) then
    Exit;

  // Exit if hander already exists
  LHandlerValue := LEventProperty.GetValue(AControl);
  if not LHandlerValue.IsEmpty then
    Exit;

  // Check handler is of type TNotifyEvent
  if LHandlerValue.TypeInfo <> TypeInfo(TNotifyEvent) then
    Exit;

  // Create new handler
  LEvent := TEvent<TNotifyEvent>.Create < TProc < TObject >> (AControl, [
  procedure(Sender: TObject)var LContext: ICoroutineExecutionContext;
  LList: IEnumerable<IResult>; begin
  // Prepare context
  LContext := TCoroutineExecutionContext.Create; LContext.Sender := Sender;
  LContext.Target := AViewModel; LContext.View := AView;

  if AViewModelMethod.ReturnType.Handle = TypeInfo(IResult) then begin
  // Execute method with return type IResult
  LResult := AViewModelMethod.Invoke(AViewModel, []).AsType<IResult>;
  LList := TList<IResult>.Create(LResult); Execute.OnBackgroundThread(
    procedure
    begin
      Execute.OnUIThread(
        procedure
        begin
          TCoroutine.BeginExecute(LList.GetEnumerator, LContext);
        end)
    end) end else if AViewModelMethod.ReturnType.Handle = TypeInfo
    (IEnumerable<IResult>) then begin
  // Execute method with return type IEnumerable<IResult>
  LList := AViewModelMethod.Invoke(AViewModel, []).AsType<IEnumerable<IResult>>;
  Execute.OnBackgroundThread(
    procedure
    begin
      Execute.OnUIThread(
        procedure
        begin
          TCoroutine.BeginExecute(LList.GetEnumerator, LContext);
        end)
    end) end; end]);

  LNotifyEvent := LEvent.Invoke;

  // Set new handler
  LHandlerValue := TValue.From<TNotifyEvent>(LNotifyEvent);
  LEventProperty.SetValue(AControl, LHandlerValue);
  Result := True;
end;

class function ViewModelBinder.BindActionsSimplified(ANamedElements
  : IEnumerable<TComponent>; AViewModel: TObject; AView: TComponent)
  : IEnumerable<TComponent>;
var
  LMethods: IEnumerable<TRttiMethod>;
  LMethod: TRttiMethod;
  LUnmatchedElements: IList<TComponent>;
  LFoundControl: TComponent;
  LViewModelType: TRttiType;
  LConvention: TElementConvention;
  LPropertyName: string;
  LProperty: TRttiProperty;
  LWired: Boolean;
begin
  LUnmatchedElements := ANamedElements.ToList();
  LViewModelType := GetRttiType(AViewModel.ClassInfo);
  LMethods := BindingScope.GetActionableMethods(LViewModelType);

  for LMethod in LMethods do
  begin
    LFoundControl := FindControlAndRemove(LUnmatchedElements, LMethod);
    if LFoundControl = nil then
      Continue;

    LConvention := ConventionManager.GetElementConvention
      (LFoundControl.ClassType);
    if not Assigned(LConvention) then
    begin
      Continue;
    end;

    LPropertyName := LFoundControl.Name;

    LWired := False;
    if Assigned(LMethod.ReturnType) and
      ((LMethod.ReturnType.Handle = TypeInfo(IResult)) or
      (LMethod.ReturnType.Handle = TypeInfo(IEnumerable<IResult>))) then
    begin
      LWired := BindNotifyEvent(LFoundControl, LConvention.EventName,
        AViewModel, AView, LMethod);
    end;

    // TODO: This block can be removed when AV in 64-bit is fixed
    // Workaround for binding TAction
    if not LWired then
    begin
      LWired := BindCustomAction(LFoundControl, AViewModel, LMethod);
    end;

    if not LWired then
    begin
      LConvention.ApplyBinding(AViewModel, LPropertyName, LFoundControl,
        btEvent, LConvention);
    end;

    LPropertyName := 'Can' + LPropertyName;
    LProperty := LViewModelType.GetProperty(LPropertyName);
    if Assigned(LProperty) then
    begin
      LConvention := TElementConvention.Create('Enabled', '');
      LConvention.ApplyBinding(AViewModel, LPropertyName, LFoundControl,
        btProperty, LConvention);
      Log.LogMessage
        ('Can___ Binding Convention Applied: %s.%s to property %s of Element %s.',
        [AViewModel.QualifiedClassName, LPropertyName, LConvention.PropertyName,
        LFoundControl.Describe()]);
      LConvention.Free;
    end;

    Log.LogMessage('Action Convention Applied: Action %s on element %s.',
      [LMethod.Describe(), LFoundControl.Describe()]);
  end;

  Result := LUnmatchedElements;
end;

class function ViewModelBinder.ApplyGlobalConventions(ANamedElements
  : IEnumerable<TComponent>; AViewModel: TObject; AView: TComponent)
  : IEnumerable<TComponent>;
type
  TPropertyCandidate = record
    NameEndingWith: string;
    MapsToPropertyName: string;
  end;
const
  CPropertyCandidates: array [0 .. 3] of TPropertyCandidate =
    ((NameEndingWith: 'Enabled'; MapsToPropertyName: 'Enabled'),
    (NameEndingWith: 'IsEnabled'; MapsToPropertyName: 'Enabled'),
    (NameEndingWith: 'Visible'; MapsToPropertyName: 'Visible'),
    (NameEndingWith: 'IsVisible'; MapsToPropertyName: 'Visible'));
var
  LPropertyCandidate: TPropertyCandidate;
  LConvention: TElementConvention;
  LElement: TComponent;
  LProperty: TRttiProperty;
  LPotentialPropertyName: string;
  LViewModelType: TRttiType;
begin
  LViewModelType := GetRttiType(AViewModel.ClassType);
  for LElement in ANamedElements do
  begin
    for LPropertyCandidate in CPropertyCandidates do
    begin
      LPotentialPropertyName := LElement.Name +
        LPropertyCandidate.NameEndingWith;
      LProperty := LViewModelType.GetProperty(LPotentialPropertyName);
      if Assigned(LProperty) then
      begin
        LConvention := TElementConvention.Create
          (LPropertyCandidate.MapsToPropertyName, '');
        LConvention.ApplyBinding(AViewModel, LPotentialPropertyName, LElement,
          btProperty, LConvention);
        LConvention.Free;
        Log.LogMessage('Global Convention Applied: %s to element %s.',
          [LPotentialPropertyName, LElement.Describe()]);
      end;
    end;
  end;
  Result := ANamedElements;
end;

class function ViewModelBinder.BindProperties(ANamedElements
  : IEnumerable<TComponent>; AViewModel: TObject; AView: TComponent)
  : IEnumerable<TComponent>;
var
  I: Integer;
  LUnmatchedElements: IList<TComponent>;
  LCleanName: string;
  LParts: TStringDynArray;
  LProperty: TRttiProperty;
  LApplied: Boolean;
  LElement: TComponent;
  LInterpretedViewModelType: TRttiType;
  LViewModelType: TRttiType;
  LConvention: TElementConvention;

  LBindingGroup: TBindingGroup;
  LElementDescription: string;
  LPropertyName: string;
begin
  LUnmatchedElements := TList<TComponent>.Create();
  LViewModelType := GetRttiType(AViewModel.ClassType);

  // TODO: refactor quick and dirty implementation
  LBindingGroup := FindBindingGroup(AView);
  if not Assigned(LBindingGroup) then
  begin
    { LBindingGroup := } TBindingGroup.Create(AView);
    // [dcc32 Hint] H2077 Value assigned to 'LBindingGroup' never used
  end;

  for LElement in ANamedElements do
  begin
    LCleanName := Trim(LElement.Name, '_');
    LParts := SplitString(LCleanName, '_');
    // TODO StringSplitOptions.RemoveEmptyEntries);

    LProperty := GetPropertyCaseInsensitive(LViewModelType, LParts[0]);

    I := 1;
    while (I < Length(LParts)) and Assigned(LProperty) do
    begin
      LInterpretedViewModelType := LProperty.PropertyType;
      LProperty := GetPropertyCaseInsensitive(LInterpretedViewModelType,
        LParts[I]);
      Inc(I);
    end;

    LElementDescription := LElement.Describe();

    if LProperty = nil then
    begin
      LUnmatchedElements.Add(LElement);
      Log.LogWarning
        ('Binding Convention Not Applied: Element %s did not match a property.',
        [LElementDescription]);
      Continue;
    end;

    LConvention := ConventionManager.GetElementConvention(LElement.ClassType);
    if LConvention = nil then
    begin
      LUnmatchedElements.Add(LElement);
      Log.LogWarning
        ('Binding Convention Not Applied: No conventions configured for %s.',
        [LElementDescription]);
      Continue;
    end;

    LPropertyName := StringReplace(LCleanName, '_', '.', [rfReplaceAll]);
    LApplied := LConvention.ApplyBinding(AViewModel, LPropertyName, LElement,
      btProperty, LConvention);

    if (LApplied) then
      Log.LogMessage
        ('Binding Convention Applied: %s.%s to property %s of Element %s.',
        [AViewModel.QualifiedClassName, LPropertyName, LConvention.PropertyName,
        LElementDescription])
    else
    begin
      Log.LogError
        ('Binding Convention Not Applied: Element %s.%s has existing binding.',
        [LElementDescription, LConvention.PropertyName]);
      LUnmatchedElements.Add(LElement);
    end;
  end;

  Result := LUnmatchedElements;
end;

class function ViewModelBinder.FindControlAndRemove(const LUnmatchedElements
  : IList<TComponent>; const LMethod: TRttiMethod): TComponent;
var
  Parent: TRttiType;
  QualifiedName: string;
begin
  Result := BindingScope.FindName(LUnmatchedElements, LMethod.Name);
  if Assigned(Result) then
    LUnmatchedElements.Remove(Result)
  else
  begin
    Parent := LMethod.Parent;
    if Assigned(Parent) then
      if Parent.IsPublicType then
      begin
        QualifiedName := Parent.QualifiedName;
        if QualifiedName.StartsWith('DSharp.PresentationModel.', True) then
          Exit;
      end;
    Log.LogWarning
      ('Action Convention Not Applied: No actionable element for %s.',
      [LMethod.Describe()]);
  end;
end;

end.
