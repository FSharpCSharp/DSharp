unit DSharp.PresentationModel.ScreenExtensions;

interface

uses
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorBase;

type
  ///	<summary>
  ///	  Hosts extension methods for <see cref="IScreen" /> classes.
  ///	</summary>
  ScreenExtensions = class

    ///	<summary>
    ///	  Activates the item if it implements <see cref="IActivate" />,
    ///	  otherwise does nothing.
    ///	</summary>
    ///	<param name="potentialActivatable">
    ///	  The potential activatable.
    ///	</param>
    class procedure TryActivate(PotentialActivatable: TValue); static;

    ///	<summary>
    ///	  Deactivates the item if it implements <see cref="IDeactivate" />,
    ///	  otherwise does nothing.
    ///	</summary>
    ///	<param name="potentialDeactivatable">
    ///	  The potential deactivatable.
    ///	</param>
    ///	<param name="close">
    ///	  Indicates whether or not to close the item after deactivating it.
    ///	</param>
    class procedure TryDeactivate(PotentialDeactivatable: TValue;
      Close: Boolean); static;

    ///	<summary>
    ///	  Closes the specified item.
    ///	</summary>
    ///	<param name="conductor">
    ///	  The conductor.
    ///	</param>
    ///	<param name="item">
    ///	  The item to close.
    ///	</param>
    class procedure CloseItem(Conductor: IConductor; Item: TValue);
      overload; static;

    ///	<summary>
    ///	  Closes the specified item.
    ///	</summary>
    ///	<param name="conductor">
    ///	  The conductor.
    ///	</param>
    ///	<param name="item">
    ///	  The item to close.
    ///	</param>
    class procedure CloseItem<T>(Conductor: TConductorBase<T>; Item: T);
      overload; static;
  end;

implementation

uses
  DSharp.Core.Reflection;

class procedure ScreenExtensions.CloseItem(Conductor: IConductor; Item: TValue);
begin
  Conductor.DeactivateItem(Item, True);
end;

class procedure ScreenExtensions.CloseItem<T>
  (Conductor: TConductorBase<T>; Item: T);
begin
  Conductor.DeactivateItem(Item, True);
end;

class procedure ScreenExtensions.TryActivate(PotentialActivatable: TValue);
var
  LActivator: IActivate;
begin
  if Supports(PotentialActivatable, IActivate, LActivator) then
  begin
    LActivator.Activate();
  end;
end;

class procedure ScreenExtensions.TryDeactivate(PotentialDeactivatable: TValue;
  Close: Boolean);
var
  LDeactivator: IDeactivate;
begin
  if Supports(PotentialDeactivatable, IDeactivate, LDeactivator) then
  begin
    LDeactivator.Deactivate(Close);
  end;
end;

end.
