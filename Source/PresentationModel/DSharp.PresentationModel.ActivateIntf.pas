unit DSharp.PresentationModel.ActivateIntf;

interface

uses
  DSharp.Core.Events,
  DSharp.PresentationModel.ActivationEventArgsIntf;

type
  ///	<summary>
  ///	  Denotes an instance which requires activation.
  ///	</summary>
  IActivate = interface
    ['{C934C697-C942-4C81-8CB6-325BC90B6295}']

    {$REGION 'Property Accessors'}
    function GetIsActive: Boolean;
    function GetActivated: IEvent<TActivationEvent>;
    {$ENDREGION}

    ///	<summary>
    ///	  Activates this instance.
    ///	</summary>
    procedure Activate();

    ///	<summary>
    ///	  Raised after activation occurs.
    ///	</summary>
    property Activated: IEvent<TActivationEvent> read GetActivated;

    ///	<summary>
    ///	  Indicates whether or not this instance is active.
    ///	</summary>
    property IsActive: Boolean read GetIsActive;
  end;

implementation

end.
