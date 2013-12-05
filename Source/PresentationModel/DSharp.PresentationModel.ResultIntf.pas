unit DSharp.PresentationModel.ResultIntf;

interface

uses
  SysUtils,
  DSharp.Core.Events,
  DSharp.PresentationModel.CoroutineExecutionContextIntf,
  DSharp.PresentationModel.ResultCompletionEventArgsIntf;

type
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
    procedure Execute(Context: ICoroutineExecutionContext);

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

implementation

end.
