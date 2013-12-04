unit DSharp.PresentationModel.ResultCompletionEventArgsIntf;

interface

uses
  SysUtils,
  DSharp.Core.EventArgs;

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

  TResultCompletionEvent = TEventHandler<IResultCompletionEventArgs>;

implementation

end.
