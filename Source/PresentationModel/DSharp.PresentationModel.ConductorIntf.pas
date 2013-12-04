unit DSharp.PresentationModel.ConductorIntf;

interface

uses
  Rtti,
  SysUtils,
  DSharp.Collections,
  DSharp.Core.EventArgs,
  DSharp.Core.Events,
  DSharp.PresentationModel.ActivationProcessedEventArgsIntf,
  DSharp.PresentationModel.ParentIntf,
  DSharp.PresentationModel.ScreenIntf;

type
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
