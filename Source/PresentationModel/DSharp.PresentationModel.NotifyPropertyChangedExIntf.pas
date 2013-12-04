unit DSharp.PresentationModel.NotifyPropertyChangedExIntf;

interface

uses
  DSharp.Bindings.Notifications;

type
  ///	<summary>
  ///	  Extends <see cref="INotifyPropertyChanged" /> such that the change
  ///	  event can be raised by external parties.
  ///	</summary>
  INotifyPropertyChangedEx = interface(INotifyPropertyChanged)
    ['{043EBAA9-082E-4438-9AB9-473E1A4B31D5}']

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
  end;

implementation

end.
