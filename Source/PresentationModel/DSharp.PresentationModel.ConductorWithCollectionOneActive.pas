unit DSharp.PresentationModel.ConductorWithCollectionOneActive;

interface

uses
  SysUtils,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorBaseWithActiveItem,
  DSharp.PresentationModel.ScreenExtensions,
  DSharp.PresentationModel.ObservableCollectionIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IConductor" /> that holds on many items
  ///	  but only activates on at a time.
  ///	</summary>
  TConductorCollectionOneActive<T> = class(TConductorBaseWithActiveItem<T>)
  strict private
    FItems: IObservableCollection<T>;
    procedure CloseItemCore(Item: T);
    procedure OnItemsCollectionChanged(Sender: TObject; const Item: T;
      Action: TCollectionChangedAction);
  strict protected
    ///	<summary>
    ///	  Determines the next item to activate based on the last active index.
    ///	</summary>
    ///	<param name="list">
    ///	  The list of possible active items.
    ///	</param>
    ///	<param name="lastIndex">
    ///	  The index of the last active item.
    ///	</param>
    ///	<returns>
    ///	  The next item to activate.
    ///	</returns>
    ///	<remarks>
    ///	  Called after an active item is closed.
    ///	</remarks>
    function DetermineNextItemToActivate(List: IList<T>; LastIndex: Integer)
      : T; virtual;

    ///	<summary>
    ///	  Called when activating.
    ///	</summary>
    procedure OnActivate; override;

    ///	<summary>
    ///	  Called when deactivating.
    ///	</summary>
    ///	<param name="close">
    ///	  Indicates whether this instance will be closed.
    ///	</param>
    procedure OnDeactivate(Close: Boolean); override;

    ///	<summary>
    ///	  Ensures that an item is ready to be activated.
    ///	</summary>
    ///	<param name="newItem">
    ///	</param>
    ///	<returns>
    ///	  The item to be activated.
    ///	</returns>
    function EnsureItem(NewItem: T): T; override;
  public
    ///	<summary>
    ///	  Initializes a new instance of the
    ///	  <see cref="Conductor&lt;T&gt;.Collection.OneActive" /> class.
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
    ///	  Deactivates the specified item.
    ///	</summary>
    ///	<param name="item">
    ///	  The item to close.
    ///	</param>
    ///	<param name="close">
    ///	  Indicates whether or not to close the item after deactivating it.
    ///	</param>
    procedure DeactivateItem(Item: T; Close: Boolean); override;

    ///	<summary>
    ///	  Called to check whether or not this instance can close.
    ///	</summary>
    ///	<param name="callback">
    ///	  The implementor calls this action with the result of the close check.
    ///	</param>
    procedure CanClose(Callback: TProc<Boolean>); override;

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
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.BindableCollection;

{ TConductorCollectionOneActive<T> }

constructor TConductorCollectionOneActive<T>.Create;
begin
  inherited Create;
  FItems := TBindableCollection<T>.Create;
  FItems.OnCollectionChanged.Add(OnItemsCollectionChanged);
end;

procedure TConductorCollectionOneActive<T>.ActivateItem(Item: T);
var
  LItem: TValue;
begin
  LItem := TValue.From<T>(Item);
  if not LItem.IsEmpty and TValue.Equals<T>(Item, ActiveItem) then
  begin
    if IsActive then
    begin
      ScreenExtensions.TryActivate(LItem);
      OnActivationProcessed(Item, True);
    end;
    Exit;
  end;
  ChangeActiveItem(Item, False);
end;

procedure TConductorCollectionOneActive<T>.CanClose(Callback: TProc<Boolean>);
begin
  CloseStrategy.Execute(FItems,
    procedure(CanClose: Boolean; Closable: IEnumerable<T>)
    var
      LItem: T;
      LList: IList<T>;
      LNext: T;
      LPrevious: T;
      LPreviousActive: T;
      LStillToClose: IList<T>;
    begin
      if not CanClose and (Closable.Count > 0) then
      begin
        if Closable.Contains(ActiveItem) then
        begin
          LList := FItems.ToList();
          LNext := ActiveItem;
          repeat
            LPrevious := LNext;
            LNext := DetermineNextItemToActivate(LList,
              LList.IndexOf(LPrevious));
            LList.Remove(LPrevious);
          until Closable.Contains(LNext);

          LPreviousActive := ActiveItem;
          ChangeActiveItem(LNext, True);
          FItems.Remove(LPreviousActive);

          LStillToClose := Closable.ToList();
          LStillToClose.Remove(LPreviousActive);
          Closable := LStillToClose;
        end;

        for LItem in Closable do
        begin
          ScreenExtensions.TryDeactivate(TValue.From<T>(LItem), True);
        end;

        FItems.RemoveRange(Closable);
      end;

      Callback(CanClose);
    end);
end;

procedure TConductorCollectionOneActive<T>.CloseItemCore(Item: T);
var
  LIndex: Integer;
  LNext: T;
begin
  if TValue.Equals<T>(Item, ActiveItem) then
  begin
    LIndex := FItems.IndexOf(Item);
    LNext := DetermineNextItemToActivate(FItems, LIndex);

    ChangeActiveItem(LNext, True);
  end
  else
  begin
    ScreenExtensions.TryDeactivate(TValue.From<T>(Item), True);
  end;

  FItems.Remove(Item);
end;

procedure TConductorCollectionOneActive<T>.DeactivateItem(Item: T;
Close: Boolean);
var
  LItem: TValue;
  LList: IList<T>;
begin
  LItem := TValue.From<T>(Item);
  if LItem.IsEmpty then
    Exit;

  if not Close then
  begin
    ScreenExtensions.TryDeactivate(LItem, False)
  end
  else
  begin
    LList := TList<T>.Create;
    LList.Add(Item);
    CloseStrategy.Execute(LList,
      procedure(CanClose: Boolean; Closable: IEnumerable<T>)
      begin
        if CanClose then
        begin
          CloseItemCore(Item);
        end;
      end);
  end;
end;

function TConductorCollectionOneActive<T>.DetermineNextItemToActivate
  (List: IList<T>; LastIndex: Integer): T;
var
  LToRemoveAt: Integer;
begin
  LToRemoveAt := LastIndex - 1;

  if (LToRemoveAt = -1) and (List.Count > 1) then
    Exit(List[1]);

  if (LToRemoveAt > -1) and (LToRemoveAt < List.Count - 1) then
    Exit(List[LToRemoveAt]);

  Result := Default (T);
end;

function TConductorCollectionOneActive<T>.EnsureItem(NewItem: T): T;
var
  LIndex: Integer;
begin
  if TValue.From<T>(NewItem).IsEmpty then
  begin
    if not TValue.From<T>(ActiveItem).IsEmpty then
    begin
      NewItem := DetermineNextItemToActivate(FItems,
        FItems.IndexOf(ActiveItem));
    end
    else
    begin
      NewItem := DetermineNextItemToActivate(FItems, 0);
    end;
  end
  else
  begin
    LIndex := FItems.IndexOf(NewItem);

    if LIndex = -1 then
    begin
      FItems.Add(NewItem);
    end
    else
    begin
      NewItem := FItems[LIndex];
    end;
  end;

  Result := inherited EnsureItem(NewItem);
end;

function TConductorCollectionOneActive<T>.GetChildren: IEnumerable<T>;
begin
  Result := FItems;
end;

procedure TConductorCollectionOneActive<T>.OnActivate;
begin
  ScreenExtensions.TryActivate(TValue.From<T>(ActiveItem));
end;

procedure TConductorCollectionOneActive<T>.OnDeactivate(Close: Boolean);
var
  LItem: T;
begin
  if Close then
  begin
    for LItem in FItems do
    begin
      ScreenExtensions.TryDeactivate(TValue.From<T>(LItem), True);
    end;
    FItems.Clear;
  end
  else
  begin
    ScreenExtensions.TryDeactivate(TValue.From<T>(ActiveItem), False);
  end;
end;

procedure TConductorCollectionOneActive<T>.OnItemsCollectionChanged
  (Sender: TObject; const Item: T; Action: TCollectionChangedAction);
var
  LChild: IChild;
  LItem: T;
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
