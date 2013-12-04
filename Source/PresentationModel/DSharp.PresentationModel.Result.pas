unit DSharp.PresentationModel.Result;

interface

uses
  SysUtils,
  DSharp.Core.EventArgs,
  DSharp.Core.Events,
  DSharp.PresentationModel.ActionExecutionContext;

type
  ///	<summary>
  ///	  The event args for the Completed event of an <see cref="IResult" />.
  ///	</summary>
  IResultCompletionEventArgs = interface(IEventArgs)
    ['{9F1B06B0-ECEF-4E16-9644-C37CB1597203}']
    {$REGION 'Property Accessors'}
    function GetError: Exception;
    function GetWasCancelled: Boolean;
    {$ENDREGION}

    ///	<summary>
    ///	  Gets or sets the error if one occurred.
    ///	</summary>
    ///	<value>
    ///	  The error.
    ///	</value>
    property Error: Exception read GetError;

    ///	<summary>
    ///	  Gets or sets a value indicating whether the result was cancelled.
    ///	</summary>
    ///	<value>
    ///	  <c>true</c> if cancelled; otherwise, <c>false</c>.
    ///	</value>
    property WasCancelled: Boolean read GetWasCancelled;
  end;

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

  TResultCompletionEvent = TEventHandler<IResultCompletionEventArgs>;

  ///	<summary>
  ///	  Allows custom code to execute after the return of a action.
  ///	</summary>

  IResult = interface
    ['{05B0E576-524B-44D3-9276-2A6279D7B0C0}']
    {$REGION 'Property Accessors'}
    function GetCompleted: IEvent<TResultCompletionEvent>;
    {$ENDREGION}

    ///	<summary>
    ///	  Executes the result using the specified context.
    ///	</summary>
    ///	<param name="Context">
    ///	  The context.
    ///	</param>
    procedure Execute(Context: IActionExecutionContext);

    ///	<summary>
    ///	  Occurs when execution has completed.
    ///	</summary>
    property Completed: IEvent<TResultCompletionEvent> read GetCompleted;
  end;

  ///	<summary>
  ///	  Allows custom code to execute after the return of a action.
  ///	</summary>
  ///	<typeparam name="TResult">
  ///	  The type of the result.
  ///	</typeparam>

  IResult<TResult> = interface(IResult)
    {$REGION 'Property Accessors'}
    function GetResult: TResult;
    {$ENDREGION}

    ///	<summary>
    ///	  Gets the result of the asynchronous operation.
    ///	</summary>
    property Result: TResult read GetResult;
  end;

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
    procedure Execute(Context: IActionExecutionContext); virtual; abstract;

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
