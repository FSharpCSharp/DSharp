unit DSharp.PresentationModel.BindingString;

interface

uses
  Classes;

type
  IBindingString = interface
    ['{2DA0F4D8-74B6-4E32-870B-52C2017F834F}']
    function GetEnumerator: TStringsEnumerator;
    function GetValue: string;
    function IsEmpty: Boolean;
    procedure SetValue(const Value: string);
    function TryGetValue(PropertyName: string;
      out PropertyValue: string): Boolean;
    property Value: string read GetValue write SetValue;
  end;

  ///	<summary>
  ///	  Parses a binding string with a syntax like the XAML binding strings
  ///	  <see href="http://msdn.microsoft.com/en-us/library/ms750413" />
  ///	</summary>
  TBindingString = class(TInterfacedObject, IBindingString)
  strict private
    FBindingStrings: TStringList;
  private
  strict protected
    function GetValue: string; virtual;
    procedure SetValue(const Value: string); virtual;
    property Value: string read GetValue write SetValue;
  public
    constructor Create(const BindingString: string);
    destructor Destroy; override;
    function GetEnumerator: TStringsEnumerator; virtual;
    function IsEmpty: Boolean; virtual;
    class function MatchesBasicBindingSyntax(const Value: string)
      : Boolean; virtual;
    function TryGetValue(PropertyName: string; out PropertyValue: string)
      : Boolean; virtual;
  end;

implementation

uses
  SysUtils,
  StrUtils;

const
  SBindingPrefix = '{';
  SBindingSuffix = '}';
  SBindingSuffixLength = Length(SBindingSuffix);
  SBindingIdentification = SBindingPrefix + 'Binding';
  SBindingIdentificationLength = Length(SBindingIdentification);
  SBindingIdentificationSeparator = ' ';
  SEmptyBindings = SBindingIdentification + SBindingSuffix;

constructor TBindingString.Create(const BindingString: string);
begin
  inherited Create();
  FBindingStrings := TStringList.Create;
  FBindingStrings.Delimiter := ',';
  SetValue(BindingString);
end;

destructor TBindingString.Destroy;
begin
  FBindingStrings.Free();
  inherited;
end;

function TBindingString.GetEnumerator: TStringsEnumerator;
begin
  Result := FBindingStrings.GetEnumerator();
end;

function TBindingString.GetValue: string;
begin
  if IsEmpty then
    Result := SEmptyBindings
  else
    Result := SBindingIdentification + SBindingIdentificationSeparator +
      FBindingStrings.DelimitedText + SBindingSuffix;
end;

function TBindingString.IsEmpty: Boolean;
begin
  Result := Length(FBindingStrings.Text) = 0;
end;

procedure TBindingString.SetValue(const Value: string);
var
  BindingsText: string;
begin
  // TODO -o##jwp -cUnitTests : write unit tests for some binding strings like '{Binding}', '{Binding Foo}', etc
  if MatchesBasicBindingSyntax(Value) then
  begin
    BindingsText := Copy(Value, SBindingIdentificationLength + 1,
      Length(Value) - SBindingIdentificationLength - SBindingSuffixLength);
    // the simplest form is '{Binding}'
    // all other forms are of form '{Binding BindingsText}' and have a space after the identification
    if '' = BindingsText then
      FBindingStrings.Clear()
    else
    begin
      if StartsText(SBindingIdentificationSeparator, BindingsText) then
        FBindingStrings.DelimitedText := BindingsText
      else
        FBindingStrings.Clear();
    end;
  end
  else
    FBindingStrings.Clear();
end;

function TBindingString.TryGetValue(PropertyName: string;
  out PropertyValue: string): Boolean;
var
  LIndex: Integer;
begin
  LIndex := FBindingStrings.IndexOfName(PropertyName);
  if LIndex >= 0 then
  begin
    Result := True;
    PropertyValue := FBindingStrings.ValueFromIndex[LIndex];
  end
  else
  begin
    Result := False;
    PropertyValue := '';
  end;
end;

class function TBindingString.MatchesBasicBindingSyntax
  (const Value: string): Boolean;
begin
  Result := StartsText(SBindingIdentification, Value) and
    EndsText(SBindingSuffix, Value);
end;

end.
