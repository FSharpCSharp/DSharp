unit DSharp.PresentationModel.DeactivateIntf;

interface

uses
  DSharp.Core.Events,
  DSharp.PresentationModel.DeactivationEventArgsIntf;

type
  ///	<summary>
  ///	  Denotes an instance which requires deactivation.
  ///	</summary>
  IDeactivate = interface
    ['{2FA4F864-94C2-46B0-83BD-9ACBDBE31EE6}']

    {$REGION 'Property Accessors'}
    function GetAttemptingDeactivation: IEvent<TDeactivationEvent>;
    function GetDeactivated: IEvent<TDeactivationEvent>;
    {$ENDREGION}

    ///	<summary>
    ///	  Deactivates this instance.
    ///	</summary>
    ///	<param name="close">
    ///	  Indicates whether or not this instance is being closed.
    ///	</param>
    procedure Deactivate(Close: Boolean);

    ///	<summary>
    ///	  Raised before deactivation.
    ///	</summary>
    property AttemptingDeactivation: IEvent<TDeactivationEvent>
      read GetAttemptingDeactivation;

    ///	<summary>
    ///	  Raised after deactivation.
    ///	</summary>
    property Deactivated: IEvent<TDeactivationEvent> read GetDeactivated;
  end;

implementation

end.
