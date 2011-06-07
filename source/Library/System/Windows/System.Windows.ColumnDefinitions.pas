unit System.Windows.ColumnDefinitions;

interface

uses
  Classes,
  Generics.Collections,
  System.Collections;

const
  CDefaultWidth = 100;

type
  TColumnDefinition = class;
  TColumnDefinitions = class;

  TColumnDefinition = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Caption: string read FCaption write FCaption;
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
  FWidth := CDefaultWidth;
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
