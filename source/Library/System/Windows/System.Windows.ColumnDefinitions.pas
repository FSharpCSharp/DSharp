unit System.Windows.ColumnDefinitions;

interface

uses
  Classes,
  Generics.Collections,
  System.Bindings,
  System.Collections;

const
  CDefaultWidth = 100;

type
  TColumnDefinition = class(TCollectionItem)
  private
    FBinding: TBinding;
    FCaption: string;
    FWidth: Integer;
    procedure SetCaption(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Binding: TBinding read FBinding;
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read FWidth write FWidth default CDefaultWidth;
  end;

  TColumnDefinitions = class(TOwnedCollection<TColumnDefinition>)
  protected
    function AddColumn(const ACaption: string; const AWidth: Integer): TColumnDefinition;
  public
    constructor Create(AOwner: TPersistent = nil); override;
  end;

implementation

{ TColumnDefinition }

constructor TColumnDefinition.Create(Collection: TCollection);
begin
  inherited;
  FBinding := TBinding.Create();
  FBinding.BindingMode := bmOneWay;
  FBinding.TargetUpdateTrigger := utExplicit;
  FWidth := CDefaultWidth;
end;

destructor TColumnDefinition.Destroy;
begin
  FBinding.Free();
  inherited;
end;

procedure TColumnDefinition.SetCaption(const Value: string);
begin
  if FCaption = FBinding.SourcePropertyName then
  begin
    FBinding.SourcePropertyName := Value;
  end;
  FCaption := Value;
end;

{ TColumnDefinitions }

constructor TColumnDefinitions.Create(AOwner: TPersistent);
begin
  inherited;
end;

function TColumnDefinitions.AddColumn(
  const ACaption: string; const AWidth: Integer): TColumnDefinition;
begin
  Result := TColumnDefinition.Create(Self);
  Result.Caption := ACaption;
  Result.Width := AWidth;
end;

end.
