unit DSharp.PresentationModel.ConductorBaseWithActiveItem;

interface

uses
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorBase,
  DSharp.PresentationModel.ScreenExtensions;

type
  ///	<summary>
  ///	  A base class for various implementations of <see cref="IConductor" />tha
  ///	   t maintain an active item.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type that is being conducted.
  ///	</typeparam>
  TConductorBaseWithActiveItem<T> = class abstract(TConductorBase<T>,
    IHaveActiveItem)
  private
    FActiveItem: T;
    procedure SetActiveItem(const Value: T); overload;
    {$REGION 'Implements IHaveActiveItem'}
    function GetActiveItem: TValue;
    procedure SetActiveItem(const Value: TValue); overload;
    {$ENDREGION}
  protected
    ///	<summary>
    ///	  Changes the active item.
    ///	</summary>
    ///	<param name="newItem">
    ///	  The new item to activate.
    ///	</param>
    ///	<param name="closePrevious">
    ///	  Indicates whether or not to close the previous active item.
    ///	</param>
    procedure ChangeActiveItem(NewItem: T; ClosePrevious: Boolean); virtual;
  public
    ///	<summary>
    ///	  The currently active item.
    ///	</summary>
    property ActiveItem: T read FActiveItem write SetActiveItem;
  end;

implementation

{ TConductorBaseWithActiveItem<T> }

procedure TConductorBaseWithActiveItem<T>.ChangeActiveItem(NewItem: T;
  ClosePrevious: Boolean);
begin
  ScreenExtensions.TryDeactivate(TValue.From<T>(FActiveItem), ClosePrevious);

  NewItem := EnsureItem(NewItem);

  if IsActive then
  begin
    ScreenExtensions.TryActivate(TValue.From<T>(NewItem));
  end;

  FActiveItem := NewItem;
  NotifyOfPropertyChange('ActiveItem');
  OnActivationProcessed(FActiveItem, True);
end;

function TConductorBaseWithActiveItem<T>.GetActiveItem: TValue;
begin
  Result := TValue.From<T>(FActiveItem);
end;

procedure TConductorBaseWithActiveItem<T>.SetActiveItem(const Value: T);
begin
  ActivateItem(Value);
end;

procedure TConductorBaseWithActiveItem<T>.SetActiveItem(const Value: TValue);
begin
  ActiveItem := Value.AsType<T>;
end;

end.
