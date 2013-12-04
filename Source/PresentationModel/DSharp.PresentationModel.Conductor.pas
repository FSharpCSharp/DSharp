unit DSharp.PresentationModel.Conductor;

interface

uses
  DSharp.Collections,
  DSharp.PresentationModel.ConductorBaseWithActiveItem,
  SysUtils;

type
  ///	<summary>
  ///	  An implementation of <see cref="IConductor" /> that holds on to and
  ///	  activates only one item at a time.
  ///	</summary>
  TConductor<T> = class(TConductorBaseWithActiveItem<T>)
  protected
    ///	<summary>
    ///	  Called when activating.
    ///	</summary>
    procedure OnActivate; override;

    ///	<summary>
    ///	  Called when deactivating.
    ///	</summary>
    ///	<param name="Close">
    ///	  Inidicates whether this instance will be closed.
    ///	</param>
    procedure OnDeactivate(Close: Boolean); override;
  public
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
    ///	  Gets all the items currently being conducted.
    ///	</summary>
    ///	<returns>
    ///	</returns>
    function GetChildren: IEnumerable<T>; override;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.ScreenExtensions,
  Rtti;

{ TConductor<T> }

procedure TConductor<T>.ActivateItem(Item: T);
var
  LList: IList<T>;
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

  LList := TList<T>.Create;
  LList.Add(ActiveItem);

  CloseStrategy.Execute(LList,
    procedure(CanClose: Boolean; Items: IEnumerable<T>)
    begin
      if CanClose then
        ChangeActiveItem(Item, True)
      else
        OnActivationProcessed(Item, False);
    end);
end;

procedure TConductor<T>.CanClose(Callback: TProc<Boolean>);
var
  LList: IList<T>;
begin
  LList := TList<T>.Create;
  LList.Add(ActiveItem);

  CloseStrategy.Execute(LList,
    procedure(CanClose: Boolean; Items: IEnumerable<T>)
    begin
      Callback(CanClose);
    end);
end;

procedure TConductor<T>.DeactivateItem(Item: T; Close: Boolean);
var
  LList: IList<T>;
  LItem: TValue;
begin
  LItem := TValue.From<T>(Item);

  if LItem.IsEmpty or not TValue.Equals<T>(Item, ActiveItem) then
    Exit;

  LList := TList<T>.Create;
  LList.Add(ActiveItem);

  CloseStrategy.Execute(LList,
    procedure(CanClose: Boolean; Items: IEnumerable<T>)
    begin
      if CanClose then
      begin
        ChangeActiveItem(Default (T), Close);
      end;
    end);
end;

function TConductor<T>.GetChildren: IEnumerable<T>;
var
  LList: IList<T>;
begin
  LList := TList<T>.Create;
  LList.Add(ActiveItem);
  Result := LList;
end;

procedure TConductor<T>.OnActivate;
begin
  ScreenExtensions.TryActivate(TValue.From<T>(ActiveItem));
end;

procedure TConductor<T>.OnDeactivate(Close: Boolean);
begin
  ScreenExtensions.TryDeactivate(TValue.From<T>(ActiveItem), Close);
end;

end.
