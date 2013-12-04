unit DSharp.PresentationModel.BindableCollection;

interface

uses
  Generics.Defaults,
  DSharp.Collections,
  DSharp.Core.Events,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.PresentationModel.ObservableCollectionIntf,
  DSharp.PresentationModel.NotifyPropertyChangedExIntf;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  A base collection class that supports automatic UI thread marshalling.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements contained in the collection.
  ///	</typeparam>
  {$ENDREGION}
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

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Sets the item at the specified position.
    ///	</summary>
    ///	<param name="Index">
    ///	  The index to set the item at.
    ///	</param>
    ///	<param name="Value">
    ///	  The item to set.
    ///	</param>
    {$ENDREGION}
    procedure SetItem(const Index: NativeInt; const Value: T); override;
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="TBindableCollection{T}" />
    ///	  class.
    ///	</summary>
    {$ENDREGION}
    constructor Create; overload;
    constructor Create(const Values: array of T); overload;
    constructor Create(Comparer: IComparer<T>); overload;
    constructor Create(Values: IEnumerable<T>); overload;
    constructor Create(Comparison: TComparison<T>); overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Adds the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    {$ENDREGION}
    procedure AddRange(Values: IEnumerable<T>);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Clears the items contained by the collection.
    ///	</summary>
    {$ENDREGION}
    procedure Clear; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Inserts the item to the specified position.
    ///	</summary>
    ///	<param name="Index">
    ///	  The index to insert at.
    ///	</param>
    ///	<param name="Value">
    ///	  The item to be inserted.
    ///	</param>
    {$ENDREGION}
    procedure Insert(const Index: NativeInt; const Value: T); override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Moves the item within the collection.
    ///	</summary>
    ///	<param name="OldIndex">
    ///	  The old position of the item.
    ///	</param>
    ///	<param name="NewIndex">
    ///	  The new position of the item.
    ///	</param>
    {$ENDREGION}
    procedure Move(const OldIndex, NewIndex: NativeInt); override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    {$ENDREGION}
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged); virtual;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Raises a change notification indicating that all bindings should be
    ///	  refreshed.
    ///	</summary>
    {$ENDREGION}
    procedure Refresh;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Removes the item at the specified position.
    ///	</summary>
    ///	<param name="index">
    ///	  The position used to identify the item to remove.
    ///	</param>
    {$ENDREGION}
    function Remove(const Value: T): NativeInt; override;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Removes the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    {$ENDREGION}
    procedure RemoveRange(Values: IEnumerable<T>);

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Enables/Disables property change notification.
    ///	</summary>
    {$ENDREGION}
    property IsNotifying: Boolean read GetIsNotifying write SetIsNotifying;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  The event for the
    ///	  <see cref="INotifyPropertyChanged.OnPropertyChanged" /> event.
    ///	</summary>
    {$ENDREGION}
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Execute;

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
