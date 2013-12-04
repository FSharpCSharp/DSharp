unit DSharp.PresentationModel.View;

interface

uses
  SysUtils,
  DSharp.Logging,
  DSharp.PresentationModel,
  DSharp.PresentationModel.RoutedEvent;

type
  ///	<summary>
  ///	  Hosts attached properties related to view models.
  ///	</summary>
  View = class
  private
  class var
    FApplyConventionsProperty: TDependencyProperty;
    FContextProperty: TDependencyProperty;
    FDefaultContentProperty: ContentPropertyAttribute;
    FDefaultContext: TValue;
    FIsGeneratedProperty: TDependencyProperty;
    FIsLoadedProperty: TDependencyProperty;
    FIsScopeRootProperty: TDependencyProperty;
    FModelProperty: TDependencyProperty;
    FLog: ILog;
    FGetFirstNonGeneratedView: TFunc<TObject, TObject>;
    FSetContentProperty: TProc<TObject, TObject>;
    class function GetLog: ILog; static;
    class procedure OnContextChanged(TargetLocation: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
    class procedure OnModelChanged(TargetLocation: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
  protected
    class property Log: ILog read GetLog;
    class property DefaultContentProperty: ContentPropertyAttribute
      read FDefaultContentProperty;
  public
    class constructor Create;
    class destructor Destroy;

    ///	<summary>
    ///	  Executes the handler immediately if the element is loaded, otherwise
    ///	  wires it to the Loaded event.
    ///	</summary>
    ///	<param name="element">
    ///	  The element.
    ///	</param>
    ///	<param name="handler">
    ///	  The handler.
    ///	</param>
    ///	<returns>
    ///	  true if the handler was executed immediately; false otherwise
    ///	</returns>
    class function ExecuteOnLoad(Element: TComponent;
      Handler: TRoutedEvent): Boolean;

    ///	<summary>
    ///	  Gets the convention application behavior.
    ///	</summary>
    ///	<param name="d">
    ///	  The element the property is attached to.
    ///	</param>
    ///	<returns>
    ///	  Whether or not to apply conventions.
    ///	</returns>
    class function GetApplyConventions(DependencyObject: TComponent): Integer;

    ///	<summary>
    ///	  Gets the context.
    ///	</summary>
    ///	<param name="d">
    ///	  The element the context is attached to.
    ///	</param>
    ///	<returns>
    ///	  The context.
    ///	</returns>
    class function GetContext(DependencyObject: TComponent): TValue;

    ///	<summary>
    ///	  Used to retrieve the root, non-framework-created view.
    ///	</summary>
    ///	<param name="view">
    ///	  The view to search.
    ///	</param>
    ///	<returns>
    ///	  The root element that was not created by the framework.
    ///	</returns>
    ///	<remarks>
    ///	  In certain instances the services create UI elements. For example, if
    ///	  you ask the window manager to show a UserControl as a dialog, it
    ///	  creates a window to host the UserControl in. The WindowManager marks
    ///	  that element as a framework-created element so that it can determine
    ///	  what it created vs. what was intended by the developer. Calling
    ///	  GetFirstNonGeneratedView allows the framework to discover what the
    ///	  original element was.
    ///	</remarks>
    class property GetFirstNonGeneratedView: TFunc<TObject, TObject>
      read FGetFirstNonGeneratedView write FGetFirstNonGeneratedView;

    ///	<summary>
    ///	  Gets the model.
    ///	</summary>
    ///	<param name="d">
    ///	  The element the model is attached to.
    ///	</param>
    ///	<returns>
    ///	  The model.
    ///	</returns>
    class function GetModel(DependencyObject: TComponent): TObject;

    ///	<summary>
    ///	  Sets the convention application behavior.
    ///	</summary>
    ///	<param name="d">
    ///	  The element to attach the property to.
    ///	</param>
    ///	<param name="value">
    ///	  Whether or not to apply conventions.
    ///	</param>
    class procedure SetApplyConventions(DependencyObject: TComponent;
      Value: Integer);

    ///	<summary>
    ///	  Sets the context.
    ///	</summary>
    ///	<param name="d">
    ///	  The element to attach the context to.
    ///	</param>
    ///	<param name="value">
    ///	  The context.
    ///	</param>
    class procedure SetContext(DependencyObject: TComponent; Value: TValue);

    ///	<summary>
    ///	  Sets the model.
    ///	</summary>
    ///	<param name="d">
    ///	  The element to attach the model to.
    ///	</param>
    ///	<param name="value">
    ///	  The model.
    ///	</param>
    class procedure SetModel(DependencyObject: TComponent; Value: TObject);

    ///	<summary>
    ///	  A dependency property which allows the override of convention
    ///	  application behavior. Values: 0=default
    ///	</summary>
    class property ApplyConventionsProperty: TDependencyProperty
      read FApplyConventionsProperty;

    ///	<summary>
    ///	  A dependency property for assigning a context to a particular portion
    ///	  of the UI.
    ///	</summary>
    class property ContextProperty: TDependencyProperty read FContextProperty;

    ///	<summary>
    ///	  The default view context.
    ///	</summary>
    class property DefaultContext: TValue read FDefaultContext;

    ///	<summary>
    ///	  Used by the framework to indicate that this element was generated.
    ///	</summary>
    class property IsGeneratedProperty: TDependencyProperty
      read FIsGeneratedProperty;

    ///	<summary>
    ///	  A dependency property which allows the framework to track whether a
    ///	  certain element has already been loaded in certain scenarios.
    ///	</summary>
    class property IsLoadedProperty: TDependencyProperty read FIsLoadedProperty;

    ///	<summary>
    ///	  A dependency property which marks an element as a name scope root.
    ///	</summary>
    class property IsScopeRootProperty: TDependencyProperty
      read FIsScopeRootProperty;

    ///	<summary>
    ///	  A dependency property for attaching a model to the UI.
    ///	</summary>
    class property ModelProperty: TDependencyProperty read FModelProperty
      write FModelProperty;

    ///	<summary>
    ///	  Set content property
    ///	</summary>
    class property SetContentProperty: TProc<TObject, TObject>
      read FSetContentProperty write FSetContentProperty;
  end;

implementation

uses
  DSharp.Core.Events,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder;

{ View }

class constructor View.Create;
begin
  FDefaultContext := '';
  FDefaultContentProperty := ContentPropertyAttribute.Create('Content');
  FApplyConventionsProperty := TDependencyProperty.RegisterAttached
    ('ApplyConventions', TypeInfo(Integer), View, nil);
  FContextProperty := TDependencyProperty.RegisterAttached('Context',
    TypeInfo(TValue), View, TPropertyMetadata.Create(nil, OnContextChanged));
  FIsGeneratedProperty := TDependencyProperty.RegisterAttached('IsGenerated',
    TypeInfo(Boolean), View, TPropertyMetadata.Create(False));
  FIsLoadedProperty := TDependencyProperty.RegisterAttached('IsLoaded',
    TypeInfo(Boolean), View, TPropertyMetadata.Create(False));
  FIsScopeRootProperty := TDependencyProperty.RegisterAttached('IsScopeRoot',
    TypeInfo(Boolean), View, TPropertyMetadata.Create(False));
  FModelProperty := TDependencyProperty.RegisterAttached('Model',
    TypeInfo(TObject), View, TPropertyMetadata.Create(nil, OnModelChanged));
  GetFirstNonGeneratedView := function(View: TObject): TObject
    begin
      raise Exception.Create('GetFirstNonGeneratedView() not assigned.');
    end;
  SetContentProperty := procedure(TargetLocation: TObject; View: TObject)
    begin
      raise Exception.Create('SetContentProperty() not assigned.');
    end;
end;

class destructor View.Destroy;
begin
  FDefaultContentProperty.Free;
end;

class function View.ExecuteOnLoad(Element: TComponent;
  Handler: TRoutedEvent): Boolean;
var
  LLoaded: Event<TRoutedEvent>;
begin
  if IsLoadedProperty.GetValue(Element).AsBoolean then
  begin
    Handler(Element, TRoutedEventArgs.Create);
    Result := True;
  end
  else
  begin
    LLoaded.Add(
      procedure(Sender: TObject; EventArgs: IRoutedEventArgs)
      begin
        IsLoadedProperty.SetValue(Element, True);
        Handler(Sender, EventArgs);
        // TODO: Element.Loaded -= LLoaded;
      end);
    // TODO: Element.Loaded += LLoaded;
    Result := False;
  end;
end;

class function View.GetApplyConventions(DependencyObject: TComponent): Integer;
begin
  Result := ApplyConventionsProperty.GetValue(DependencyObject).AsInteger;
end;

class function View.GetContext(DependencyObject: TComponent): TValue;
begin
  Result := ContextProperty.GetValue(DependencyObject);
end;

class function View.GetLog: ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(View));
  end;
  Result := FLog;
end;

class function View.GetModel(DependencyObject: TComponent): TObject;
begin
  Result := ModelProperty.GetValue(DependencyObject).AsObject;
end;

class procedure View.OnContextChanged(TargetLocation: TComponent;
EventArgs: IDependencyPropertyChangedEventArgs);
var
  LModel: TObject;
  LView: TComponent;
begin
  if SameValue(EventArgs.OldValue, EventArgs.NewValue) then
    Exit;

  LModel := GetModel(TargetLocation);
  if LModel = nil then
    Exit;

  LView := ViewLocator.LocateForModel(LModel, TargetLocation,
    EventArgs.NewValue);

  SetContentProperty(TargetLocation, LView);
  ViewModelBinder.Bind(LModel, LView, EventArgs.NewValue);
end;

class procedure View.OnModelChanged(TargetLocation: TComponent;
EventArgs: IDependencyPropertyChangedEventArgs);
var
  LContext: TValue;
  LView: TComponent;
begin
  if SameValue(EventArgs.OldValue, EventArgs.NewValue) then
    Exit;

  if not EventArgs.NewValue.IsEmpty then
  begin
    LContext := GetContext(TargetLocation);
    LView := ViewLocator.LocateForModel(EventArgs.NewValue.AsObject,
      TargetLocation, LContext);

    SetContentProperty(TargetLocation, LView);
    ViewModelBinder.Bind(EventArgs.NewValue.AsObject, LView, LContext);
  end
  else
    SetContentProperty(TargetLocation, EventArgs.NewValue.AsObject);
end;

class procedure View.SetApplyConventions(DependencyObject: TComponent;
Value: Integer);
begin
  ApplyConventionsProperty.SetValue(DependencyObject, Value);
end;

class procedure View.SetContext(DependencyObject: TComponent; Value: TValue);
begin
  ContextProperty.SetValue(DependencyObject, Value);
end;

class procedure View.SetModel(DependencyObject: TComponent; Value: TObject);
begin
  ModelProperty.SetValue(DependencyObject, Value);
end;

end.
