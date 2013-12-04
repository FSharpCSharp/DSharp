unit DSharp.PresentationModel.ActivationProcessedEventArgs;

interface

uses
  Rtti,
  DSharp.Core.EventArgs,
  DSharp.PresentationModel.ActivationProcessedEventArgsIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IActivationProcessedEventArgs" />
  ///	</summary>
  TActivationProcessedEventArgs = class(TEventArgs,
    IActivationProcessedEventArgs)
  private
    FItem: TValue;
    FSuccess: Boolean;
    function GetItem: TValue;
    function GetSuccess: Boolean;
  public
    constructor Create(Item: TValue; Success: Boolean);
  end;

implementation

{ TActivationProcessedEventArgs }

constructor TActivationProcessedEventArgs.Create(Item: TValue;
  Success: Boolean);
begin
  FItem := Item;
  FSuccess := Success;
end;

function TActivationProcessedEventArgs.GetItem: TValue;
begin
  Result := FItem;
end;

function TActivationProcessedEventArgs.GetSuccess: Boolean;
begin
  Result := FSuccess;
end;

end.
