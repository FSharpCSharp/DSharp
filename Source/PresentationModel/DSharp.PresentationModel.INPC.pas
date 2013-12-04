unit DSharp.PresentationModel.INPC;

interface

uses
  Classes,
  SysUtils,
  Windows,
  Generics.Defaults,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Core.Events,
  DSharp.Collections,
  DSharp.Collections.ObservableCollection;

type

  ///	<summary>
  ///	  Enables easy marshalling of code to the UI / background thread.
  ///	</summary>
  Execute = class
  private
  class var
    FBackgroundThreadExecutor: TProc<TProc>;
    FUIThreadExecutor: TProc<TProc>;
  public
    class constructor Create;

    ///	<summary>
    ///	  Initializes the framework using the current dispatcher.
    ///	</summary>
    class procedure InitializeWithDispatcher; static;

    ///	<summary>
    ///	  Executes the action on the background thread.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure OnBackgroundThread(Action: TProc); static;

    ///	<summary>
    ///	  Executes the action on the UI thread.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure OnUIThread(Action: TProc); static;

    ///	<summary>
    ///	  Executes the action on the UI thread. But it makes sure this is
    ///	  called from the background thread.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure QueueActionOnUIThread(Action: TProc); static;

    ///	<summary>
    ///	  Resets the executor to use a non-dispatcher-based action executor.
    ///	</summary>
    class procedure ResetWithoutDispatcher; static;

    ///	<summary>
    ///	  Sets a custom background thread marshaller.
    ///	</summary>
    ///	<param name="Marshaller">
    ///	  The marshaller.
    ///	</param>
    class procedure SetBackgroundThreadMarshaller
      (Marshaller: TProc<TProc>); static;

    ///	<summary>
    ///	  Sets a custom UI thread marshaller.
    ///	</summary>
    ///	<param name="Marshaller">
    ///	  The marshaller.
    ///	</param>
    class procedure SetUIThreadMarshaller(Marshaller: TProc<TProc>); static;
  end;

  ///	<summary>
  ///	  Extends <see cref="INotifyPropertyChanged" /> such that the change
  ///	  event can be raised by external parties.
  ///	</summary>

  INotifyPropertyChangedEx = interface(INotifyPropertyChanged)
    ['{043EBAA9-082E-4438-9AB9-473E1A4B31D5}']

    {$REGION 'Property Accessors'}
    function GetIsNotifying: Boolean;
    procedure SetIsNotifying(const Value: Boolean);
    {$ENDREGION}

    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged);

    ///	<summary>
    ///	  Raises a change notification indicating that all bindings should be
    ///	  refreshed.
    ///	</summary>
    procedure Refresh;

    ///	<summary>
    ///	  Enables/Disables property change notification.
    ///	</summary>
    property IsNotifying: Boolean read GetIsNotifying write SetIsNotifying;
  end;

  ///	<summary>
  ///	  A base class that implements the infrastructure for property change
  ///	  notification and automatically performs UI thread marshalling.
  ///	</summary>

  TPropertyChangedBase = class(TComponent, IInterface, INotifyPropertyChanged,
    INotifyPropertyChangedEx)
  private
    FIsNotifying: Boolean;
    FPropertyChanged: Event<TPropertyChangedEvent>;
    FRefCount: Integer;
    procedure DoPropertyChanged(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged);
    function GetIsNotifying: Boolean;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    procedure SetIsNotifying(const Value: Boolean);
  protected
    // IInterface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    ///	<summary>
    ///	  Creates an instance of <see cref="TPropertyChangedBase" />.
    ///	</summary>
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;

    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged); virtual;

    ///	<summary>
    ///	  Raises a change notification indicating that all bindings should be
    ///	  refreshed.
    ///	</summary>
    procedure Refresh;

    ///	<summary>
    ///	  Enables/Disables property change notification.
    ///	</summary>
    property IsNotifying: Boolean read GetIsNotifying write SetIsNotifying;

    ///	<summary>
    ///	  The event for the
    ///	  <see cref="INotifyPropertyChanged.OnPropertyChanged" /> event.
    ///	</summary>
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
  end;

  ///	<summary>
  ///	  Represents a collection that is observable.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements contained in the collection.
  ///	</typeparam>

  IObservableCollection<T> = interface(IList<T>)
    // Also expose INotifyPropertyChangedEx
    {$REGION 'Property Accessors'}
    function GetIsNotifying: Boolean;
    procedure SetIsNotifying(const Value: Boolean);
    {$ENDREGION}

    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged);

    ///	<summary>
    ///	  Raises a change notification indicating that all bindings should be
    ///	  refreshed.
    ///	</summary>
    procedure Refresh;

    ///	<summary>
    ///	  Enables/Disables property change notification.
    ///	</summary>
    property IsNotifying: Boolean read GetIsNotifying write SetIsNotifying;

    ///	<summary>
    ///	  Adds the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    procedure AddRange(Values: IEnumerable<T>);

    ///	<summary>
    ///	  Removes the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    procedure RemoveRange(Values: IEnumerable<T>);
  end;

  ///	<summary>
  ///	  A base collection class that supports automatic UI thread marshalling.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements contained in the collection.
  ///	</typeparam>

  TBindableCollection<T> = class(TList<T>, IObservableCollection<T>,
    INotifyCollectionChanged, INotifyPropertyChangedEx, INotifyPropertyChanged)
  private
    FIsNotifying: Boolean;
    FOnPropertyChanged: Event<TPropertyChangedEvent>;
    function GetIsNotifying: Boolean;
    function GetOnCollectionChanged: IEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    procedure SetIsNotifying(const Value: Boolean);
  protected
    procedure DoItemPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged); virtual;
    procedure Notify(const Value: T;
      const Action: TCollectionChangedAction); override;

    ///	<summary>
    ///	  Sets the item at the specified position.
    ///	</summary>
    ///	<param name="Index">
    ///	  The index to set the item at.
    ///	</param>
    ///	<param name="Value">
    ///	  The item to set.
    ///	</param>
    procedure SetItem(const Index: NativeInt; const Value: T); override;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="TBindableCollection{T}" />
    ///	  class.
    ///	</summary>
    constructor Create; overload;
    constructor Create(const Values: array of T); overload;
    constructor Create(Comparer: IComparer<T>); overload;
    constructor Create(Values: IEnumerable<T>); overload;
    constructor Create(Comparison: TComparison<T>); overload;

    ///	<summary>
    ///	  Adds the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    procedure AddRange(Values: IEnumerable<T>);

    ///	<summary>
    ///	  Clears the items contained by the collection.
    ///	</summary>
    procedure Clear; override;

    ///	<summary>
    ///	  Inserts the item to the specified position.
    ///	</summary>
    ///	<param name="Index">
    ///	  The index to insert at.
    ///	</param>
    ///	<param name="Value">
    ///	  The item to be inserted.
    ///	</param>
    procedure Insert(const Index: NativeInt; const Value: T); override;

    ///	<summary>
    ///	  Moves the item within the collection.
    ///	</summary>
    ///	<param name="OldIndex">
    ///	  The old position of the item.
    ///	</param>
    ///	<param name="NewIndex">
    ///	  The new position of the item.
    ///	</param>
    procedure Move(const OldIndex, NewIndex: NativeInt); override;

    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged); virtual;

    ///	<summary>
    ///	  Raises a change notification indicating that all bindings should be
    ///	  refreshed.
    ///	</summary>
    procedure Refresh;

    ///	<summary>
    ///	  Removes the item at the specified position.
    ///	</summary>
    ///	<param name="index">
    ///	  The position used to identify the item to remove.
    ///	</param>
    function Remove(const Value: T): NativeInt; override;

    ///	<summary>
    ///	  Removes the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    procedure RemoveRange(Values: IEnumerable<T>);

    ///	<summary>
    ///	  Enables/Disables property change notification.
    ///	</summary>
    property IsNotifying: Boolean read GetIsNotifying write SetIsNotifying;

    ///	<summary>
    ///	  The event for the
    ///	  <see cref="INotifyPropertyChanged.OnPropertyChanged" /> event.
    ///	</summary>
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ Execute }

class constructor Execute.Create;
begin
  FUIThreadExecutor := procedure(Action: TProc)
    begin
      Action();
    end;
  FBackgroundThreadExecutor := procedure(Action: TProc)
    begin
      Action();
    end;
end;

class procedure Execute.InitializeWithDispatcher;
begin
  SetUIThreadMarshaller(
    procedure(Action: TProc)
    begin
      if GetCurrentThreadId() = MainThreadID then
      begin
        Action();
      end
      else
      begin
        TThread.Queue(nil,
          procedure
          begin
            Action();
          end);
      end;
    end);

  SetBackgroundThreadMarshaller(
    procedure(Action: TProc)
    begin
      TThread.CreateAnonymousThread(
        procedure
        begin
          Action();
        end).Start;
    end);
end;

class procedure Execute.OnBackgroundThread(Action: TProc);
begin
  FBackgroundThreadExecutor(Action);
end;

class procedure Execute.OnUIThread(Action: TProc);
begin
  FUIThreadExecutor(Action);
end;

class procedure Execute.QueueActionOnUIThread(Action: TProc);
begin
  FBackgroundThreadExecutor(
    procedure
    begin
      FUIThreadExecutor(Action);
    end);
end;

class procedure Execute.ResetWithoutDispatcher;
begin
  SetUIThreadMarshaller(
    procedure(Action: TProc)
    begin
      Action();
    end);

  SetBackgroundThreadMarshaller(
    procedure(Action: TProc)
    begin
      Action();
    end);
end;

class procedure Execute.SetBackgroundThreadMarshaller(Marshaller: TProc<TProc>);
begin
  FBackgroundThreadExecutor := Marshaller;
end;

class procedure Execute.SetUIThreadMarshaller(Marshaller: TProc<TProc>);
begin
  FUIThreadExecutor := Marshaller;
end;

{ TPropertyChangedBase }

constructor TPropertyChangedBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsNotifying := True;
end;

procedure TPropertyChangedBase.AfterConstruction;
begin
  Dec(FRefCount);
end;

procedure TPropertyChangedBase.DoPropertyChanged(const PropertyName: string;
UpdateTrigger: TUpdateTrigger);
begin
  if FPropertyChanged.Count > 0 then
  begin
    FPropertyChanged.Invoke(Self, PropertyName, UpdateTrigger);
  end;
end;

function TPropertyChangedBase.GetIsNotifying: Boolean;
begin
  Result := FIsNotifying;
end;

function TPropertyChangedBase.GetOnPropertyChanged
  : IEvent<TPropertyChangedEvent>;
begin
  Result := FPropertyChanged;
end;

class function TPropertyChangedBase.NewInstance: TObject;
var
  LInterfaceTable: PInterfaceTable;
begin
  Result := inherited NewInstance;
  LInterfaceTable := Result.GetInterfaceTable;
  if Assigned(LInterfaceTable) and (LInterfaceTable.EntryCount > 0) then
  begin
    TPropertyChangedBase(Result).FRefCount := 1;
  end
  else
  begin
    TPropertyChangedBase(Result).FRefCount := 2;
  end;
end;

procedure TPropertyChangedBase.NotifyOfPropertyChange(const PropertyName
  : string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
begin
  if IsNotifying then
  begin
    Execute.OnUIThread(
      procedure
      begin
        DoPropertyChanged(PropertyName, UpdateTrigger);
      end);
  end;
end;

procedure TPropertyChangedBase.Refresh;
begin
  NotifyOfPropertyChange('');
end;

procedure TPropertyChangedBase.SetIsNotifying(const Value: Boolean);
begin
  FIsNotifying := Value;
end;

function TPropertyChangedBase._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TPropertyChangedBase._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result = 0 then
  begin
    if not(csDestroying in ComponentState) then
      Destroy;
  end;
end;

{ TBindableCollection<T> }

constructor TBindableCollection<T>.Create;
begin
  IsNotifying := True;
  inherited;
end;

constructor TBindableCollection<T>.Create(const Values: array of T);
begin
  IsNotifying := True;
  inherited;
end;

constructor TBindableCollection<T>.Create(Comparer: IComparer<T>);
begin
  IsNotifying := True;
  inherited;
end;

constructor TBindableCollection<T>.Create(Values: IEnumerable<T>);
begin
  IsNotifying := True;
  inherited;
end;

constructor TBindableCollection<T>.Create(Comparison: TComparison<T>);
begin
  IsNotifying := True;
  inherited;
end;

procedure TBindableCollection<T>.AddRange(Values: IEnumerable<T>);
begin
  Execute.OnUIThread(
    procedure
    var
      LPreviousNotificationSetting: Boolean;
    begin
      LPreviousNotificationSetting := IsNotifying;
      IsNotifying := False;
      InsertRange(Count, Values);
      IsNotifying := LPreviousNotificationSetting;

      DoPropertyChanged('', utPropertyChanged);
      Notify(Default (T), caReset);
    end);
end;

procedure TBindableCollection<T>.Clear;
begin
  Execute.OnUIThread(
    procedure
    begin
      inherited;
    end);
end;

procedure TBindableCollection<T>.DoItemPropertyChanged(ASender: TObject;
APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  inherited Notify(TValue.From(ASender).AsType<T>, caReplace);
end;

procedure TBindableCollection<T>.DoPropertyChanged(const APropertyName: string;
AUpdateTrigger: TUpdateTrigger);
begin
  if IsNotifying then
  begin
    FOnPropertyChanged.Invoke(Self, APropertyName);
  end;
end;

function TBindableCollection<T>.GetIsNotifying: Boolean;
begin
  Result := FIsNotifying;
end;

function TBindableCollection<T>.GetOnCollectionChanged
  : IEvent<TCollectionChangedEvent>;
begin
  IEvent < TCollectionChangedEvent < T >> (Result) :=
    inherited OnCollectionChanged;
end;

function TBindableCollection<T>.GetOnPropertyChanged
  : IEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged;
end;

procedure TBindableCollection<T>.Insert(const Index: NativeInt; const Value: T);
begin
  Execute.OnUIThread(
    procedure
    begin
      inherited;
    end);
end;

procedure TBindableCollection<T>.Move(const OldIndex, NewIndex: NativeInt);
begin
  Execute.OnUIThread(
    procedure
    begin
      inherited;
    end);
end;

procedure TBindableCollection<T>.Notify(const Value: T;
const Action: TCollectionChangedAction);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  if IsNotifying then
  begin
    if Supports(TValue.From<T>(Value), INotifyPropertyChanged,
      LNotifyPropertyChanged) then
    begin
      LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
      case Action of
        caAdd:
          LPropertyChanged.Add(DoItemPropertyChanged);
        caRemove, caExtract:
          LPropertyChanged.Remove(DoItemPropertyChanged);
      end;
    end;

    inherited;
    DoPropertyChanged('Count');
  end;
end;

procedure TBindableCollection<T>.NotifyOfPropertyChange(const PropertyName
  : string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
begin
  if IsNotifying then
  begin
    Execute.OnUIThread(
      procedure
      begin
        DoPropertyChanged(PropertyName, UpdateTrigger);
      end);
  end;
end;

procedure TBindableCollection<T>.Refresh;
begin
  Execute.OnUIThread(
    procedure
    begin
      DoPropertyChanged('', utPropertyChanged);
      Notify(Default (T), caReset);
    end);
end;

function TBindableCollection<T>.Remove(const Value: T): NativeInt;
begin
  Execute.OnUIThread(
    procedure
    begin
      inherited;
    end);
end;

procedure TBindableCollection<T>.RemoveRange(Values: IEnumerable<T>);
begin
  Execute.OnUIThread(
    procedure
    var
      LItem: T;
      LPreviousNotificationSetting: Boolean;
    begin
      LPreviousNotificationSetting := IsNotifying;
      IsNotifying := False;
      for LItem in Values do
      begin
        Remove(LItem);
      end;
      IsNotifying := LPreviousNotificationSetting;

      DoPropertyChanged('', utPropertyChanged);
      Notify(Default (T), caReset);
    end);
end;

procedure TBindableCollection<T>.SetIsNotifying(const Value: Boolean);
begin
  FIsNotifying := Value;
end;

procedure TBindableCollection<T>.SetItem(const Index: NativeInt;
const Value: T);
begin
  Execute.OnUIThread(
    procedure
    begin
      inherited;
    end);
end;

end.
