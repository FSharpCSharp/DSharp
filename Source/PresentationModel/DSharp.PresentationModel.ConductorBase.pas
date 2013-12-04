unit DSharp.PresentationModel.ConductorBase;

interface

uses
  DSharp.Collections,
  DSharp.Core.Events,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorIntf,
  DSharp.PresentationModel.CloseStrategyIntf,
  DSharp.PresentationModel.ParentIntf;

type
  TConductorBase = class(TScreen, IParent)
  private
    {$REGION 'Implements IParent'}
    function GetChildren: IEnumerable; reintroduce;
    function NonGenericGetChildren: IEnumerable; virtual; abstract;
    {$ENDREGION}
  end;

  ///	<summary>
  ///	  A base class for various implementations of <see cref="IConductor" />.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type that is being conducted.
  ///	</typeparam>

  TConductorBase<T> = class(TConductorBase, IConductor, IParent<T>)
  private
    FActivationProcessed: Event<TActivationProcessedEvent>;
    FCloseStrategy: ICloseStrategy<T>;
    function GetCloseStrategy: ICloseStrategy<T>;

    {$REGION 'Implements IConductor'}
    /// <summary>
    /// Activates the specified item.
    /// </summary>
    /// <param name="item">The item to activate.</param>
    procedure IConductor.ActivateItem = NonGenericActivateItem;
    procedure NonGenericActivateItem(Item: TValue); overload;

    ///	<summary>
    ///	  Closes the specified item.
    ///	</summary>
    ///	<param name="item">
    ///	  The item to close.
    ///	</param>
    procedure DeactivateItem(Item: TValue; Close: Boolean); overload;
    function GetActivationProcessed: IEvent<TActivationProcessedEvent>;
    {$ENDREGION}
    function NonGenericGetChildren: IEnumerable; override;
  protected
    ///	<summary>
    ///	  Ensures that an item is ready to be activated.
    ///	</summary>
    ///	<param name="newItem">
    ///	</param>
    ///	<returns>
    ///	  The item to be activated.
    ///	</returns>
    function EnsureItem(NewItem: T): T; virtual;

    ///	<summary>
    ///	  Called by a subclass when an activation needs processing.
    ///	</summary>
    ///	<param name="Item">
    ///	  The AItem on which activation was attempted.
    ///	</param>
    ///	<param name="Success">
    ///	  if set to <c>true</c> activation was successful.
    ///	</param>
    procedure OnActivationProcessed(Item: T; Success: Boolean); virtual;
  public
    ///	<summary>
    ///	  Gets or sets the close strategy.
    ///	</summary>
    ///	<value>
    ///	  The close strategy.
    ///	</value>
    property CloseStrategy: ICloseStrategy<T> read GetCloseStrategy
      write FCloseStrategy;

    ///	<summary>
    ///	  Activates the specified item.
    ///	</summary>
    ///	<param name="item">
    ///	  The item to activate.
    ///	</param>
    procedure ActivateItem(Item: T); overload; virtual; abstract;

    ///	<summary>
    ///	  Deactivates the specified item.
    ///	</summary>
    ///	<param name="item">
    ///	  The item to close.
    ///	</param>
    ///	<param name="close">
    ///	  Indicates whether or not to close the item after deactivating it.
    ///	</param>
    procedure DeactivateItem(Item: T; Close: Boolean); overload;
      virtual; abstract;

    {$REGION 'Implements IParent<T>'}

    ///	<summary>
    ///	  Gets the children.
    ///	</summary>
    function GetChildren: IEnumerable<T>; virtual; abstract;
    {$ENDREGION}

    ///	<summary>
    ///	  Occurs when an activation request is processed.
    ///	</summary>
    property ActivationProcessed: IEvent<TActivationProcessedEvent>
      read GetActivationProcessed;
  end;

implementation

uses
  DSharp.Core.Reflection, 
  DSharp.PresentationModel.DefaultCloseStrategy;

{ TConductorBase }

function TConductorBase.GetChildren: IEnumerable;
begin
  Result := NonGenericGetChildren;
end;

{ TConductorBase<T> }

procedure TConductorBase<T>.DeactivateItem(Item: TValue; Close: Boolean);
begin
  DeactivateItem(Item.AsType<T>, Close);
end;

function TConductorBase<T>.EnsureItem(NewItem: T): T;
var
  LNode: IChild;
begin
  if Supports(TValue.From<T>(NewItem), IChild, LNode) then
  begin
    if Assigned(LNode) and (LNode.Parent.AsObject <> Self) then
    begin
      LNode.Parent := Self;
    end;
  end;

  Result := NewItem;
end;

function TConductorBase<T>.GetActivationProcessed
  : IEvent<TActivationProcessedEvent>;
begin
  Result := FActivationProcessed;
end;

function TConductorBase<T>.GetCloseStrategy: ICloseStrategy<T>;
begin
  if not Assigned(FCloseStrategy) then
  begin
    FCloseStrategy := TDefaultCloseStrategy<T>.Create;
  end;

  Result := FCloseStrategy;
end;

procedure TConductorBase<T>.NonGenericActivateItem(Item: TValue);
begin
  ActivateItem(Item.AsType<T>);
end;

function TConductorBase<T>.NonGenericGetChildren: IEnumerable;
begin
  Result := GetChildren;
end;

procedure TConductorBase<T>.OnActivationProcessed(Item: T; Success: Boolean);
var
  LArgs: IActivationProcessedEventArgs;
begin
  if TValue.From<T>(Item).IsEmpty then
    Exit;

  LArgs := TActivationProcessedEventArgs.Create(TValue.From<T>(Item), Success);
  ActivationProcessed.Invoke(Self, LArgs);
end;

end.
