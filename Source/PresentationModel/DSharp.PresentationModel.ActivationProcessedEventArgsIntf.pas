unit DSharp.PresentationModel.ActivationProcessedEventArgsIntf;

interface

uses
  Rtti,
  DSharp.Core.EventArgs;

type
  ///	<summary>
  ///	  Contains details about the success or failure of an item's activation
  ///	  through an <see cref="IConductor" />.
  ///	</summary>
  IActivationProcessedEventArgs = interface(IEventArgs)
    ['{21B5DB7B-125B-41F1-BFDE-D58228369E23}']

    {$REGION 'Property Accessors'}
    function GetItem: TValue;
    function GetSuccess: Boolean;
    {$ENDREGION}

    ///	<summary>
    ///	  The item whose activation was processed.
    ///	</summary>
    property Item: TValue read GetItem;

    ///	<summary>
    ///	  Gets or sets a value indicating whether the activation was a success.
    ///	</summary>
    ///	<value>
    ///	  <c>true</c> if success; otherwise, <c>false</c>.
    ///	</value>
    property Success: Boolean read GetSuccess;
  end;

  TActivationProcessedEvent = TEventHandler<IActivationProcessedEventArgs>;

implementation

end.
