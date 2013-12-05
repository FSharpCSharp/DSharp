unit DSharp.PresentationModel.ResultBase;

interface

uses
  DSharp.Core.Events,
  DSharp.PresentationModel.CoroutineExecutionContextIntf,
  DSharp.PresentationModel.ResultIntf,
  DSharp.PresentationModel.ResultCompletionEventArgsIntf;

type
  ///	<summary>
  ///	  Base class for implementation of <see cref="IResult" />.
  ///	</summary>
  TResultBase = class(TInterfacedObject, IResult)
  strict private
    FCompleted: Event<TResultCompletionEvent>;

    function GetCompleted: IEvent<TResultCompletionEvent>;
  public
    ///	<summary>
    ///	  Executes the result using the specified context.
    ///	</summary>
    ///	<param name="Context">
    ///	  The context.
    ///	</param>
    procedure Execute(Context: ICoroutineExecutionContext); virtual; abstract;

    ///	<summary>
    ///	  Occurs when execution has completed.
    ///	</summary>
    property Completed: IEvent<TResultCompletionEvent> read GetCompleted;
  end;

implementation

{ TResultBase }

function TResultBase.GetCompleted: IEvent<TResultCompletionEvent>;
begin
  Result := FCompleted;
end;

end.
