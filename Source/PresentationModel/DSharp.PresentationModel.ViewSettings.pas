unit DSharp.PresentationModel.ViewSettings;

interface

uses
  SysUtils,
  Rtti,
  Generics.Collections;

type
  ///	<summary>
  ///	  Represents a collection of default properties and values for the view
  ///	</summary>
  IViewSettings = interface
    ['{E1B1FE2C-3320-4712-922E-5068D1CD84E3}']
    function GetItems: TDictionary<string, TValue>;
    function GetValues(Name: string): TValue;
    procedure SetValues(Name: string; const Value: TValue);
    property Items: TDictionary<string, TValue> read GetItems;
    property Values[Name: string]: TValue read GetValues
      write SetValues; default;
  end;

  ///	<summary>
  ///	  An implementation of <see cref="IViewSettings" />
  ///	</summary>
  TViewSettings = class(TInterfacedObject, IViewSettings)
  private
    FItems: TDictionary<string, TValue>;
    function GetItems: TDictionary<string, TValue>;
    function GetValues(Name: string): TValue;
    procedure SetValues(Name: string; const Value: TValue);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TViewSettings.Create;
begin
  FItems := TDictionary<string, TValue>.Create;
end;

destructor TViewSettings.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TViewSettings.GetItems: TDictionary<string, TValue>;
begin
  Result := FItems;
end;

function TViewSettings.GetValues(Name: string): TValue;
begin
  Result := FItems.Items[Name];
end;

procedure TViewSettings.SetValues(Name: string; const Value: TValue);
begin
  FItems.AddOrSetValue(Name, Value);
end;

end.
