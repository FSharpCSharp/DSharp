unit System.PropertyChangedBase;

interface

uses
  Classes,
  System.Bindings,
  System.Events;

type
  TPropertyChangedBase = class abstract(TInterfacedPersistent, INotifyPropertyChanged)
  private
    FPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  end;

implementation

{ TPropertyChangedBase }

procedure TPropertyChangedBase.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

function TPropertyChangedBase.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FPropertyChanged.EventHandler;
end;

end.
