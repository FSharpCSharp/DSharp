unit DSharp.PresentationModel.ActivationEventArgs;

interface

uses
  DSharp.Core.EventArgs,
  DSharp.PresentationModel.ActivationEventArgsIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IActivationEventArgs" />
  ///	</summary>
  TActivationEventArgs = class(TEventArgs, IActivationEventArgs)
  private
    FWasInitialized: Boolean;
    function GetWasInitialized: Boolean;
  public
    constructor Create(WasInitialized: Boolean);
  end;

implementation

{ TActivationEventArgs }

constructor TActivationEventArgs.Create(WasInitialized: Boolean);
begin
  FWasInitialized := WasInitialized;
end;

function TActivationEventArgs.GetWasInitialized: Boolean;
begin
  Result := FWasInitialized;
end;

end.
