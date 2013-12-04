unit DSharp.PresentationModel.ActivationEventArgsIntf;

interface

uses
  DSharp.Core.EventArgs;

type
  ///	<summary>
  ///	  EventArgs sent during activation.
  ///	</summary>
  IActivationEventArgs = interface(IEventArgs)
    ['{4C15F89C-1E08-4B43-BD60-C12ECCCC85DB}']
    function GetWasInitialized: Boolean;

    ///	<summary>
    ///	  Indicates whether the sender was initialized in addition to being
    ///	  activated.
    ///	</summary>
    property WasInitialized: Boolean read GetWasInitialized;
  end;

  TActivationEvent = TEventHandler<IActivationEventArgs>;

implementation

end.
