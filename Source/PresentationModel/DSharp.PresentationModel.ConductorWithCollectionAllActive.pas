unit DSharp.PresentationModel.ConductorWithCollectionAllActive;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorBaseWithActiveItem,
  Generics.Defaults,
  Spring.Helpers,
  Spring.Reflection,
  DSharp.Collections,
  DSharp.PresentationModel.ScreenExtensions,
  DSharp.PresentationModel.ObservableCollectionIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IConductor" /> that holds on to many
  ///	  items wich are all activated.
  ///	</summary>
  TConductorCollectionAllActive<T> = class(TConductorBaseWithActiveItem<T>)
  private
    FItems: IObservableCollection<T>;
    FOpenPublicItems: Boolean;
    procedure CloseItemCore(Item: T);
    procedure OnItemsCollectionChanged(Sender: TObject; const Item: T;
      Action: TCollectionChangedAction);
  protected
    ///	<summary>
    ///	  Ensures that an item is ready to be activated.
    ///	</summary>
    ///	<param name="NewItem">
    ///	</param>
    ///	<returns>
    ///	  The item to be activated.
    ///	</returns>
    function EnsureItem(NewItem: T): T; override;

    ///	<summary>
    ///	  Called when activating.
    ///	</summary>
    procedure OnActivate; override;

    ///	<summary>
    ///	  Called when deactivating.
    ///	</summary>
    ///	<param name="Close">
    ///	  Indicates whether this instance will be closed.
    ///	</param>
    procedure OnDeactivate(Close: Boolean); override;

    ///	<summary>
    ///	  Called when initializing.
    ///	</summary>
    procedure OnInitialize; override;
  public
    ///	<summary>
    ///	  Initializes a new instance of the
    ///	  <see cref="Conductor&lt;T&gt;.Collection.AllActive" /> class.
    ///	</summary>
    constructor Create; override;

    ///	<summary>
    ///	  Activates the specified Item.
    ///	</summary>
    ///	<param name="Item">
    ///	  The Item to activate.
    ///	</param>
    procedure ActivateItem(Item: T); override;

    ///	<summary>
    ///	  Called to check whether or not this instance can close.
    ///	</summary>
    ///	<param name="Callback">
    ///	  The implementor calls this action with the result of the close check.
    ///	</param>
    procedure CanClose(Callback: TProc<Boolean>); override;

    ///	<summary>
    ///	  Deactivates the specified item.
    ///	</summary>
    ///	<param name="Item">
    ///	  The item to close.
    ///	</param>
    ///	<param name="Close">
    ///	  Indicates whether or not to close the item after deactivating it.
    ///	</param>
    procedure DeactivateItem(Item: T; Close: Boolean); override;

    ///	<summary>
    ///	  Gets the children.
    ///	</summary>
    ///	<returns>
    ///	  The collection of children.
    ///	</returns>
    function GetChildren: IEnumerable<T>; override;

    ///	<summary>
    ///	  Gets the items that are currently being conducted.
    ///	</summary>
    property Items: IObservableCollection<T> read FItems;

    ///	<summary>
    ///	  Activate all public items
    ///	</summary>
    ///	<value>
    ///	  If set to &lt;c&gt;true&lt;/c&gt; opens public items that are
    ///	  properties of this class.
    ///	</value>
    ///	<remarks>
    ///	  Set this property inside constructor.
    ///	</remarks>
    property OpenPublicItems: Boolean read FOpenPublicItems
      write FOpenPublicItems;
  end;

implementation

uses
  Rtti,
  DSharp.Core.Reflection,
  DSharp.PresentationModel.BindableCollection;

{ TConductorCollectionAllActive<T> }

constructor TConductorCollectionAllActive<T>.Create;
begin
  inherited Create();
  FItems := TBindableCollection<T>.Create;
  FItems.OnCollectionChanged.Add(OnItemsCollectionChanged);
  FOpenPublicItems := False;
end;

procedure TConductorCollectionAllActive<T>.ActivateItem(Item: T);
var
  LItem: TValue;
begin
  LItem := TValue.From<T>(Item);
  if LItem.IsEmpty then
    Exit;

  Item := EnsureItem(Item);

  if IsActive then
  begin
    ScreenExtensions.TryActivate(LItem);
  end;

  OnActivationProcessed(Item, True);
end;

procedure TConductorCollectionAllActive<T>.CanClose(Callback: TProc<Boolean>);
begin
  CloseStrategy.Execute(FItems,
    procedure(_CanClose: Boolean; _Closable: IEnumerable<T>)
    var
      LItem: T;
    begin
      if (not _CanClose) and (_Closable.Count > 0) then
      begin
        for LItem in _Closable do
          ScreenExtensions.TryDeactivate(TValue.From<T>(LItem), True);

        FItems.RemoveRange(_Closable);
      end;

      Callback(_CanClose);
    end);
end;

procedure TConductorCollectionAllActive<T>.CloseItemCore(Item: T);
begin
  ScreenExtensions.TryDeactivate(TValue.From<T>(Item), True);
  FItems.Remove(Item);
end;

procedure TConductorCollectionAllActive<T>.DeactivateItem(Item: T;
Close: Boolean);
var
  LItem: TValue;
  LList: IList<T>;
begin
  LItem := TValue.From<T>(Item);
  if LItem.IsEmpty then
    Exit;

  if not Close then
    ScreenExtensions.TryDeactivate(LItem, False)
  else
  begin
    LList := TList<T>.Create;
    LList.Add(Item);
    CloseStrategy.Execute(LList,
      procedure(_CanClose: Boolean; _Closable: IEnumerable<T>)
      begin
        if (_CanClose) then
          CloseItemCore(Item);
      end);
  end;
end;

function TConductorCollectionAllActive<T>.EnsureItem(NewItem: T): T;
var
  LIndex: Integer;
begin
  LIndex := FItems.IndexOf(NewItem);

  if (LIndex = -1) then
    FItems.Add(NewItem)
  else
    NewItem := FItems[LIndex];

  Result := inherited EnsureItem(NewItem);
end;

function TConductorCollectionAllActive<T>.GetChildren: IEnumerable<T>;
begin
  Result := FItems;
end;

procedure TConductorCollectionAllActive<T>.OnActivate;
var
  LItem: T;
begin
  for LItem in Items do
    ScreenExtensions.TryActivate(TValue.From<T>(LItem));
end;

procedure TConductorCollectionAllActive<T>.OnDeactivate(Close: Boolean);
var
  LItem: T;
begin
  for LItem in Items do
    ScreenExtensions.TryDeactivate(TValue.From<T>(LItem), Close);

  if Close then
  begin
    Items.Clear;
  end;
end;

procedure TConductorCollectionAllActive<T>.OnInitialize;
var
  LProperty: TRttiProperty;
  LItem: T;
begin
  if FOpenPublicItems then
  begin
    for LProperty in GetRttiType(Self.ClassType).GetProperties do
    begin
      if (LProperty.Name <> 'Parent') and
        TType.IsAssignable(LProperty.PropertyType.Handle, TypeInfo(T)) then
      begin
        LItem := LProperty.GetValue(Self).AsType<T>;
        ActivateItem(LItem);
      end;
    end;
  end;
end;

procedure TConductorCollectionAllActive<T>.OnItemsCollectionChanged
  (Sender: TObject; const Item: T; Action: TCollectionChangedAction);
var
  LItem: T;
  LChild: IChild;
begin
  case Action of
    caAdd:
      begin
        if Supports(TValue.From<T>(Item), IChild, LChild) then
        begin
          LChild.Parent := Self;
        end;
      end;
    caRemove:
      begin
        if Supports(TValue.From<T>(Item), IChild, LChild) then
        begin
          LChild.Parent := nil;
        end;
      end;
    caReplace:
      begin
        if Supports(TValue.From<T>(Item), IChild, LChild) then
        begin
          LChild.Parent := Self;
        end;
      end;
    caReset:
      begin
        for LItem in FItems do
        begin
          if Supports(TValue.From<T>(LItem), IChild, LChild) then
            LChild.Parent := Self;
        end;
      end;
  end;
end;

end.
