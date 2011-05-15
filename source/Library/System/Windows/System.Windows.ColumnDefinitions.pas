unit System.Windows.ColumnDefinitions;

interface

uses
  Generics.Collections;

type
  TColumnDefinition = class;
  TColumnDefinitions = class;

  TColumnDefinition = class
  private
    FCaption: string;
    FWidth: Integer;
  public
    property Caption: string read FCaption write FCaption;
    property Width: Integer read FWidth write FWidth;
  end;

  TColumnDefinitions = class
  private
    FColumnDefinitions: TObjectList<TColumnDefinition>;
    function GetItem(Index: Integer): TColumnDefinition;
    function GetCount: Integer;
  protected
    function AddColumn(const ACaption: string; const AWidth: Integer): TColumnDefinition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetColumnDefinitions; virtual;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TColumnDefinition read GetItem; default;
  end;

implementation

{ TColumnDefinitions }

constructor TColumnDefinitions.Create;
begin
  FColumnDefinitions := TObjectList<TColumnDefinition>.Create(True);
  GetColumnDefinitions();
end;

destructor TColumnDefinitions.Destroy;
begin
  FColumnDefinitions.Free();
  inherited;
end;

procedure TColumnDefinitions.GetColumnDefinitions;
begin

end;

function TColumnDefinitions.GetCount: Integer;
begin
  Result := FColumnDefinitions.Count;
end;

function TColumnDefinitions.GetItem(Index: Integer): TColumnDefinition;
begin
  Result := FColumnDefinitions.Items[Index] as TColumnDefinition;
end;

function TColumnDefinitions.AddColumn(
  const ACaption: string; const AWidth: Integer): TColumnDefinition;
begin
  Result := TColumnDefinition.Create;
  Result.Caption := ACaption;
  Result.Width := AWidth;
  FColumnDefinitions.Add(Result);
end;

end.
