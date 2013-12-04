unit DSharp.PresentationModel.ViewAware;

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  DSharp.Core.DependencyProperty,
  DSharp.PresentationModel.INPC,
  DSharp.PresentationModel.ViewAttachedEventArgsIntf,
  DSharp.PresentationModel.ViewAwareIntf,
  DSharp.Core.Events;

type
  ///	<summary>
  ///	  A base implementation of <see cref="IViewAware" /> which is capable of
  ///	  caching views by context.
  ///	</summary>
  TViewAware = class(TPropertyChangedBase, IViewAware)
  const
    ///	<summary>
    ///	  Indicates whether or not implementors of <see cref="IViewAware" />shou
    ///	   ld cache their views by default.
    ///	</summary>
    CacheViewsByDefault = True;
  private
    FCacheViews: Boolean;
    FSkipNotification: Boolean;
    FViewAttached: Event<TViewAttachedEvent>;
    FViews: TDictionary<TValue, TObject>;
    procedure OnViewsValueNotify(Sender: TObject; const Item: TObject;
      Action: TCollectionNotification);
  class var
    class var FPreviouslyAttachedProperty: TDependencyProperty;

    ///	<summary>
    ///	  Attaches a view to this instance.
    ///	</summary>
    ///	<param name="view">
    ///	  The view.
    ///	</param>
    ///	<param name="context">
    ///	  The context in which the view appears.
    ///	</param>
    procedure AttachView(View: TObject); overload;
    procedure AttachView(AView: TObject; AContext: TValue); overload;
    function GetViewAttached: IEvent<TViewAttachedEvent>;
    procedure SetCacheViews(const Value: Boolean);
    class property PreviouslyAttachedProperty: TDependencyProperty
      read FPreviouslyAttachedProperty;
  protected
    ///	<summary>
    ///	  Called when a view is attached.
    ///	</summary>
    ///	<param name="view">
    ///	  The view.
    ///	</param>
    ///	<param name="context">
    ///	  The context in which the view appears.
    ///	</param>
    procedure OnViewAttached(View: TObject; Context: TValue); virtual;

    ///	<summary>
    ///	  Called when an attached view's Loaded event fires.
    ///	</summary>
    ///	<param name="view">
    ///	</param>
    procedure OnViewLoaded(View: TObject); virtual;

    ///	<summary>
    ///	  Indicates whether or not this instance maintains a view cache.
    ///	</summary>
    property CacheViews: Boolean read FCacheViews write SetCacheViews;

    ///	<summary>
    ///	  The view chache for this instance.
    ///	</summary>
    property Views: TDictionary<TValue, TObject> read FViews;

    ///	<summary>
    ///	  Called when view (owned by this instance) is destroyed by it's parent
    ///	  (when parent is destroyed it also destroys all of its children)
    ///	</summary>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    class constructor Create;

    ///	<summary>
    ///	  Creates an instance of <see cref="ViewAware" />.
    ///	</summary>
    constructor Create; reintroduce; overload; virtual;
    destructor Destroy; override;

    ///	<summary>
    ///	  Gets a view previously attached to this instance.
    ///	</summary>
    ///	<param name="context">
    ///	  The context denoting which view to retrieve.
    ///	</param>
    ///	<returns>
    ///	  The view.
    ///	</returns>
    function GetView: TObject; overload;
    function GetView(AContext: TValue): TObject; overload;
    procedure TakeOwnership(const View: TObject);

    ///	<summary>
    ///	  Raised when a view is attached.
    ///	</summary>
    property ViewAttached: IEvent<TViewAttachedEvent> read GetViewAttached;
  end;

implementation

uses
  DSharp.PresentationModel.RoutedEvent,
  DSharp.PresentationModel.View,
  DSharp.PresentationModel.ViewAttachedEventArgs;

class constructor TViewAware.Create;
begin
  FPreviouslyAttachedProperty := TDependencyProperty.RegisterAttached
    ('PreviouslyAttached', TypeInfo(Boolean), TViewAware);
end;

constructor TViewAware.Create;
begin
  inherited Create(nil);
  FViews := TDictionary<TValue, TObject>.Create(
    // Dictionary with a key of TValue must be supported by custom comparer to give correct results
    // Context (TValue) is usually some simple type easily represented by string value (enumeration, string,...)
    TEqualityComparer<TValue>.Construct(
    function(const Left, Right: TValue): Boolean
    begin
      Result := CompareStr(Left.ToString, Right.ToString) = 0;
    end,
    function(const Value: TValue): Integer
    var
      LValue: string;
    begin
      { Generate a hash code. }
      LValue := Value.ToString;
      Result := BobJenkinsHash(PChar(LValue)^,
        SizeOf(Char) * Length(LValue), 0);
    end));
  FViews.OnValueNotify := OnViewsValueNotify;
  FSkipNotification := False;
  CacheViews := CacheViewsByDefault;
end;

destructor TViewAware.Destroy;
begin
  FViews.Free;
  inherited;
end;

{ TViewAware }

procedure TViewAware.AttachView(View: TObject);
begin
  AttachView(View, nil);
end;

procedure TViewAware.AttachView(AView: TObject; AContext: TValue);
var
  LNonGeneratedView: TObject;
  LElement: TComponent;
begin
  if AContext.IsEmpty then
    AContext := View.DefaultContext;

  if CacheViews then
  begin
    if not Views.ContainsKey(AContext) then
      Views.Add(AContext, AView);
  end;

  LNonGeneratedView := View.GetFirstNonGeneratedView(AView);

  LElement := LNonGeneratedView as TComponent;
  if Assigned(LElement) and not PreviouslyAttachedProperty.GetValue(LElement).AsBoolean
  then
  begin
    PreviouslyAttachedProperty.SetValue(LElement, True);

    View.ExecuteOnLoad(LElement,
      procedure(Sender: TObject; EventArgs: IRoutedEventArgs)
      begin
        OnViewLoaded(Sender);
      end);
  end;

  OnViewAttached(LNonGeneratedView, AContext);
  ViewAttached.Invoke(Self, TViewAttachedEventArgs.Create(LNonGeneratedView,
    AContext) as IViewAttachedEventArgs);
end;

function TViewAware.GetView: TObject;
begin
  Result := GetView(nil);
end;

function TViewAware.GetView(AContext: TValue): TObject;
begin
  if AContext.IsEmpty then
    AContext := View.DefaultContext;
  FViews.TryGetValue(AContext, Result);
end;

function TViewAware.GetViewAttached: IEvent<TViewAttachedEvent>;
begin
  Result := FViewAttached;
end;

procedure TViewAware.Notification(AComponent: TComponent;
Operation: TOperation);
var
  LPair: TPair<TValue, TObject>;
begin
  inherited;

  if FSkipNotification then
    Exit;

  if Operation = opRemove then
  begin
    // Remove destroyed view from view cache
    for LPair in FViews do
    begin
      if LPair.Value = AComponent then
      begin
        FViews.ExtractPair(LPair.Key);
      end;
    end;
  end;
end;

procedure TViewAware.OnViewAttached(View: TObject; Context: TValue);
begin
end;

procedure TViewAware.OnViewLoaded(View: TObject);
begin
end;

procedure TViewAware.OnViewsValueNotify(Sender: TObject; const Item: TObject;
Action: TCollectionNotification);
var
  LView: TComponent;
begin
  if (Action = cnRemoved) then
  begin
    LView := Item as TComponent;
    // Skip auto generated containers (like TChildForm)
    if View.IsGeneratedProperty.GetValue(LView).AsBoolean then
      Exit;

    // Skip views that have assigned different owner
    if Assigned(LView.Owner) and (LView.Owner <> Self) then
      Exit;

    // Skip currently destroying views
    if csDestroying in LView.ComponentState then
      Exit;

    // Destroy cached view
    LView.Free;
  end;
end;

procedure TViewAware.SetCacheViews(const Value: Boolean);
begin
  FCacheViews := Value;
  if not FCacheViews then
    Views.Clear;
end;

procedure TViewAware.TakeOwnership(const View: TObject);
begin
  FSkipNotification := True;
  try
    InsertComponent(View as TComponent);
  finally
    FSkipNotification := False;
  end;
end;

end.
