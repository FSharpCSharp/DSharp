unit DSharp.PresentationModel.HandleWithCoroutineIntf;

interface

uses
  DSharp.PresentationModel.EventAggregatorIntf,
  DSharp.PresentationModel.Result;

type
  ///	<summary>
  ///	  Denotes a class which can handle a particular type of message and uses
  ///	  a TCoroutine to do so.
  ///	</summary>
  IHandleWithCoroutine<TMessage> = interface(IHandle)

    ///	<summary>
    ///	  Handle the message with a TCoroutine.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message.
    ///	</param>
    ///	<returns>
    ///	  The TCoroutine to execute.
    ///	</returns>
    function Handle(AMessage: TMessage): IEnumerable<IResult>;
  end;

implementation

end.
