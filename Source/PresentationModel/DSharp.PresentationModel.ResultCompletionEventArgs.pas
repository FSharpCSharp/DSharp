unit DSharp.PresentationModel.ResultCompletionEventArgs;

interface

uses
  SysUtils,
  DSharp.Core.EventArgs,
  DSharp.PresentationModel.ResultCompletionEventArgsIntf;

type
  TResultCompletionEventArgs = class(TEventArgs, IResultCompletionEventArgs)
  private
    FError: Exception;
    FWasCancelled: Boolean;
    function GetError: Exception;
    function GetWasCancelled: Boolean;
  public
    constructor Create; overload;
    constructor Create(Error: Exception; WasCancelled: Boolean); overload;
  end;

implementation

constructor TResultCompletionEventArgs.Create(Error: Exception;
  WasCancelled: Boolean);
begin
  FError := Error;
  FWasCancelled := WasCancelled;
end;

constructor TResultCompletionEventArgs.Create;
begin
end;

function TResultCompletionEventArgs.GetError: Exception;
begin
  Result := FError;
end;

function TResultCompletionEventArgs.GetWasCancelled: Boolean;
begin
  Result := FWasCancelled;
end;

end.
