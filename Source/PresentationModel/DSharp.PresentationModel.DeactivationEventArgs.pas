unit DSharp.PresentationModel.DeactivationEventArgs;

interface

uses
  DSharp.Core.EventArgs,
  DSharp.PresentationModel.DeactivationEventArgsIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IDeactivationEventArgs" />
  ///	</summary>
  TDeactivationEventArgs = class(TEventArgs, IDeactivationEventArgs)
  private
    FWasClosed: Boolean;
    function GetWasClosed: Boolean;
  public
    constructor Create(WasClosed: Boolean);
  end;

implementation

{ TDeactivationEventArgs }

constructor TDeactivationEventArgs.Create(WasClosed: Boolean);
begin
  FWasClosed := WasClosed;
end;

function TDeactivationEventArgs.GetWasClosed: Boolean;
begin
  Result := FWasClosed;
end;

end.
