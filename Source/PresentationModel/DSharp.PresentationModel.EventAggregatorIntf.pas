unit DSharp.PresentationModel.EventAggregatorIntf;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  DSharp.ComponentModel.Composition;

type
  ///	<summary>
  ///	  A marker interface for classes that subscribe to messages.
  ///	</summary>
  {$M+}
  IHandle = interface
    ['{B65C8B5C-C1C1-4BE9-B620-82AD419CAA13}']
  end;
  {$M-}

  ///	<summary>
  ///	  Denotes a class which can handle a particular type of message.
  ///	</summary>
  ///	<typeparam name="TMessage">
  ///	  The type of message to handle.
  ///	</typeparam>
  IHandle<TMessage> = interface(IHandle)

    ///	<summary>
    ///	  Handles the message.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message.
    ///	</param>
    procedure Handle(AMessage: TMessage);
  end;

  ///	<summary>
  ///	  Enables loosely-coupled publication of and subscription to events.
  ///	</summary>
  [InheritedExport]
  IEventAggregator = interface
    ['{61E862A0-E1D4-4DBD-B479-4678D8E2E569}']

    ///	<summary>
    ///	  Searches the subscribed handlers to check if we have a handler for
    ///	  the message type supplied.
    ///	</summary>
    ///	<param name="MessageType">
    ///	  The message type to check with
    ///	</param>
    ///	<returns>
    ///	  True if any handler is found, false if not.
    ///	</returns>
    function HandlerExistsFor(MessageType: PTypeInfo): Boolean;

    ///	<summary>
    ///	  Subscribes an instance to all events declared through implementations
    ///	  of <see cref="IHandle{T}" />
    ///	</summary>
    ///	<param name="Subscriber">
    ///	  The instance to subscribe for event publication.
    ///	</param>
    procedure Subscribe(Subscriber: TObject);

    ///	<summary>
    ///	  Unsubscribes the instance from all events.
    ///	</summary>
    ///	<param name="Subscriber">
    ///	  The instance to unsubscribe.
    ///	</param>
    procedure Unsubscribe(Subscriber: TObject);

    ///	<summary>
    ///	  Publishes a message.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    ///	<param name="Marshal">
    ///	  Allows the publisher to provide a custom thread marshaller for the
    ///	  message publication.
    ///	</param>
    procedure Publish(AMessage: TValue; Marshal: TProc<TProc>;
      AutoFree: Boolean = True); overload;

    ///	<summary>
    ///	  Publishes a message on the current thread (synchronous).
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure PublishOnCurrentThread(const AMessage: TValue);

    ///	<summary>
    ///	  Publishes a message on a background thread (async).
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure PublishOnBackgroundThread(const AMessage: TValue);

    ///	<summary>
    ///	  Publishes a message on the UI thread.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure PublishOnUIThread(const AMessage: TValue);

    ///	<summary>
    ///	  Publishes a message on the UI thread (asynchronous) without waiting
    ///	  for it to complete.
    ///	</summary>
    ///	<param name="AMessage">
    ///	  The message instance.
    ///	</param>
    procedure BeginPublishOnUIThread(const AMessage: TValue);

  end;

implementation

end.
