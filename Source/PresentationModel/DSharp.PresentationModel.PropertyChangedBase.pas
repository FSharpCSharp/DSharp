unit DSharp.PresentationModel.PropertyChangedBase;

interface

uses
  Classes,
  DSharp.Bindings.Notifications,
  DSharp.Core.Events,
  DSharp.PresentationModel.NotifyPropertyChangedExIntf;

type
  ///	<summary>
  ///	  A base class that implements the infrastructure for property change
  ///	  notification and automatically performs UI thread marshalling.
  ///	</summary>
  TPropertyChangedBase = class(TComponent, IInterface, INotifyPropertyChanged,
    INotifyPropertyChangedEx)
  private
    FIsNotifying: Boolean;
    FPropertyChanged: Event<TPropertyChangedEvent>;
    FRefCount: Integer;
    procedure DoPropertyChanged(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged);
    function GetIsNotifying: Boolean;
    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    procedure SetIsNotifying(const Value: Boolean);
  protected
    // IInterface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    ///	<summary>
    ///	  Creates an instance of <see cref="TPropertyChangedBase" />.
    ///	</summary>
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;

    ///	<summary>
    ///	  Notifies subscribers of the property change.
    ///	</summary>
    ///	<param name="PropertyName">
    ///	  Name of the property.
    ///	</param>
    procedure NotifyOfPropertyChange(const PropertyName: string;
      UpdateTrigger: TUpdateTrigger = utPropertyChanged); virtual;

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
    ///	  The event for the
    ///	  <see cref="INotifyPropertyChanged.OnPropertyChanged" /> event.
    ///	</summary>
    property OnPropertyChanged: IEvent<TPropertyChangedEvent>
      read GetOnPropertyChanged;
  end;

implementation

uses
  DSharp.PresentationModel.Execute;

{ TPropertyChangedBase }

constructor TPropertyChangedBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsNotifying := True;
end;

procedure TPropertyChangedBase.AfterConstruction;
begin
  Dec(FRefCount);
end;

procedure TPropertyChangedBase.DoPropertyChanged(const PropertyName: string;
  UpdateTrigger: TUpdateTrigger);
begin
  if FPropertyChanged.Count > 0 then
  begin
    FPropertyChanged.Invoke(Self, PropertyName, UpdateTrigger);
  end;
end;

function TPropertyChangedBase.GetIsNotifying: Boolean;
begin
  Result := FIsNotifying;
end;

function TPropertyChangedBase.GetOnPropertyChanged
  : IEvent<TPropertyChangedEvent>;
begin
  Result := FPropertyChanged;
end;

class function TPropertyChangedBase.NewInstance: TObject;
var
  LInterfaceTable: PInterfaceTable;
begin
  Result := inherited NewInstance;
  LInterfaceTable := Result.GetInterfaceTable;
  if Assigned(LInterfaceTable) and (LInterfaceTable.EntryCount > 0) then
  begin
    TPropertyChangedBase(Result).FRefCount := 1;
  end
  else
  begin
    TPropertyChangedBase(Result).FRefCount := 2;
  end;
end;

procedure TPropertyChangedBase.NotifyOfPropertyChange(const PropertyName
  : string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
begin
  if IsNotifying then
  begin
    Execute.OnUIThread(
      procedure
      begin
        DoPropertyChanged(PropertyName, UpdateTrigger);
      end);
  end;
end;

procedure TPropertyChangedBase.Refresh;
begin
  NotifyOfPropertyChange('');
end;

procedure TPropertyChangedBase.SetIsNotifying(const Value: Boolean);
begin
  FIsNotifying := Value;
end;

function TPropertyChangedBase._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TPropertyChangedBase._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result = 0 then
  begin
    if not(csDestroying in ComponentState) then
      Destroy;
  end;
end;

end.
