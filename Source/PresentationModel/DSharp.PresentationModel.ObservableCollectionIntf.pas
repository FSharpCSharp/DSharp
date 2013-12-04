unit DSharp.PresentationModel.ObservableCollectionIntf;

interface

uses
  DSharp.Collections,
  DSharp.Bindings.Notifications;

type
  ///	<summary>
  ///	  Represents a collection that is observable.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements contained in the collection.
  ///	</typeparam>
  IObservableCollection<T> = interface(IList<T>)
    // Also expose INotifyPropertyChangedEx
    {$REGION 'Property Accessors'}
    function GetIsNotifying: Boolean;
    procedure SetIsNotifying(const Value: Boolean);
    {$ENDREGION}

    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged);

    ///	<summary>
    ///	  Raises a change notification indicating that all bindings should be
    ///	  refreshed.
    ///	</summary>
    procedure Refresh;

    ///	<summary>
    ///	  Enables/Disables property change notification.
    ///	</summary>
    property IsNotifying: Boolean read GetIsNotifying write SetIsNotifying;

    ///	<summary>
    ///	  Adds the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    procedure AddRange(Values: IEnumerable<T>);

    ///	<summary>
    ///	  Removes the range.
    ///	</summary>
    ///	<param name="Values">
    ///	  The items.
    ///	</param>
    procedure RemoveRange(Values: IEnumerable<T>);
  end;

implementation

end.
