unit DSharp.PresentationModel.ConductorIntf;

interface

uses
  Rtti,
  SysUtils,
  DSharp.Collections,
  DSharp.Core.EventArgs,
  DSharp.Core.Events,
  DSharp.PresentationModel.ActivationProcessedEventArgsIntf,
  DSharp.PresentationModel.ScreenIntf;

type
  ///	<summary>
  ///	  Interface used to define an object associated to a collection of
  ///	  children.
  ///	</summary>
  IParent = interface
    ['{6FA2F654-73C2-4C76-A640-271791F2FAC4}']

    ///	<summary>
    ///	  Gets the children.
    ///	</summary>
    ///	<returns>
    ///	  The collection of children.
    ///	</returns>
    function GetChildren: IEnumerable;
  end;

  ///	<summary>
  ///	  Interface used to define a specialized parent.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of children.
  ///	</typeparam>
  IParent<T> = interface(IParent)

    ///	<summary>
    ///	  Gets the children.
    ///	</summary>
    ///	<returns>
    ///	  The collection of children.
    ///	</returns>
    function GetChildren: IEnumerable<T>;
  end;

  // IChildOfConductor = interface(IChild<IConductor>)
  // ['{2B36C4F7-3B66-4835-B44E-312BE0FB94CD}']
  // end;

  ///	<summary>
  ///	  Denotes an instance which maintains an active item.
  ///	</summary>
  IHaveActiveItem = interface
    ['{F849A40D-7075-4573-B804-F750C37AEF60}']

    {$REGION 'Property Accessors'}
    function GetActiveItem: TValue;
    procedure SetActiveItem(const Value: TValue);
    {$ENDREGION}

    ///	<summary>
    ///	  The currently active item.
    ///	</summary>
    property ActiveItem: TValue read GetActiveItem write SetActiveItem;
  end;

  ///	<summary>
  ///	  Denotes an instance which conducts other objects by managing an
  ///	  ActiveItem and maintaining a strict lifecycle.
  ///	</summary>
  ///	<remarks>
  ///	  Conducted instances can opt-in to the lifecycle by implementing any of
  ///	  the following <see cref="IActivate" />, <see cref="IDeactivate" />,
  ///	  <see cref="IGuardClose" />
  ///	</remarks>
  IConductor = interface(IParent)
    ['{D18BAB64-BBB3-48C2-B4DB-66A346E9227E}']

    {$REGION 'Property Accessors'}
    function GetActivationProcessed: IEvent<TActivationProcessedEvent>;
    {$ENDREGION}

    ///	<summary>
    ///	  Activates the specified item.
    ///	</summary>
    ///	<param name="item">
    ///	  The item to activate.
    ///	</param>
    procedure ActivateItem(Item: TValue);

    ///	<summary>
    ///	  Closes the specified item.
    ///	</summary>
    ///	<param name="item">
    ///	  The item to close.
    ///	</param>
    procedure DeactivateItem(Item: TValue; Close: Boolean);

    ///	<summary>
    ///	  Occurs when an activation request is processed.
    ///	</summary>
    property ActivationProcessed: IEvent<TActivationProcessedEvent>
      read GetActivationProcessed;
  end;

implementation

end.
