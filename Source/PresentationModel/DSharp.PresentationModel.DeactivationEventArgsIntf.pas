unit DSharp.PresentationModel.DeactivationEventArgsIntf;

interface

uses
  DSharp.Core.EventArgs;

type
  ///	<summary>
  ///	  EventArgs sent during deactivation.
  ///	</summary>
  IDeactivationEventArgs = interface(IEventArgs)
    ['{504F362B-A045-49A0-8B0F-273C84921EE0}']
    function GetWasClosed: Boolean;

    ///	<summary>
    ///	  Indicates whether the sender was closed in addition to being
    ///	  deactivated.
    ///	</summary>
    property WasClosed: Boolean read GetWasClosed;
  end;

  TDeactivationEvent = TEventHandler<IDeactivationEventArgs>;

implementation

end.
