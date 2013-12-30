unit DSharp.PresentationModel.NameTransformer;

interface

uses
  SysUtils,
  DSharp.Collections;

type
  /// <summary>
  ///   A rule that describes a name transform.
  /// </summary>
  TRule = class
  private
    FGlobalFilterPattern: string;
    FReplacePattern: string;
    FReplacementValues: IEnumerable<string>;
  public
    constructor Create(const ReplacePattern: string; const ReplacementValues: IEnumerable<string>;
      const GlobalFilterPattern: string);

    /// <summary>
    ///   Regular expression pattern for global filtering
    /// </summary>
    property GlobalFilterPattern: string read FGlobalFilterPattern;

    /// <summary>
    ///   Regular expression pattern for replacing text
    /// </summary>
    property ReplacePattern: string read FReplacePattern;

    /// <summary>
    ///   The list of replacement values
    /// </summary>
    property ReplacementValues: IEnumerable<string> read FReplacementValues;
  end;

  /// <summary>
  ///   Interface for managing the list of rules for doing name transformation.
  /// </summary>
  INameTransformer = interface
    ['{8B7CA360-5B7C-4C3C-A4E7-CF4771CD76C7}']

    {$REGION 'Property Accessors'}
    function GetUseEagerRuleSelection: Boolean;
    procedure SetUseEagerRuleSelection(const Value: Boolean);
{$ENDREGION}
    /// <summary>
    ///   Adds a transform using a single replacement value and a global filter
    ///   pattern.
    /// </summary>
    /// <param name="ReplacePattern">
    ///   Regular expression pattern for replacing text
    /// </param>
    /// <param name="ReplaceValue">
    ///   The replacement value.
    /// </param>
    /// <param name="GlobalFilterPattern">
    ///   Regular expression pattern for global filtering
    /// </param>
    procedure AddRule(ReplacePattern, ReplaceValue: string; const GlobalFilterPattern: string = ''); overload;

    /// <summary>
    ///   Adds a transform using a list of replacement values and a global
    ///   filter pattern.
    /// </summary>
    /// <param name="ReplacePattern">
    ///   Regular expression pattern for replacing text
    /// </param>
    /// <param name="ReplaceValueList">
    ///   The list of replacement values
    /// </param>
    /// <param name="GlobalFilterPattern">
    ///   Regular expression pattern for global filtering
    /// </param>
    procedure AddRule(const ReplacePattern: string; const ReplaceValueList: IEnumerable<string>;
      const GlobalFilterPattern: string = ''); overload;

    procedure Clear;

    /// <summary>
    ///   Gets the list of transformations for a given name.
    /// </summary>
    /// <param name="Source">
    ///   The name to transform into the resolved name list
    /// </param>
    /// <returns>
    ///   The transformed names.
    /// </returns>
    function Transform(const Source: string): IEnumerable<string>; overload;

    /// <summary>
    ///   Gets the list of transformations for a given name.
    /// </summary>
    /// <param name="Source">
    ///   The name to transform into the resolved name list
    /// </param>
    /// <param name="GetReplaceString">
    ///   A function to do a transform on each item in the ReplaceValueList
    ///   prior to applying the regular expression transform
    /// </param>
    /// <returns>
    ///   The transformed names.
    /// </returns>
    function Transform(const Source: string; GetReplaceString: TFunc<string, string>): IEnumerable<string>; overload;

    /// <summary>
    ///   Flag to indicate if transformations from all matched rules are
    ///   returned. Otherwise, transformations from only the first matched rule
    ///   are returned.
    /// </summary>
    property UseEagerRuleSelection: Boolean read GetUseEagerRuleSelection write SetUseEagerRuleSelection;
  end;

  /// <summary>
  ///   Class for managing the list of rules for doing name transformation.
  /// </summary>
  TNameTransformer = class(TObjectList<TRule>, INameTransformer)
  private
    FUseEagerRuleSelection: Boolean;
    function GetUseEagerRuleSelection: Boolean;
    procedure SetUseEagerRuleSelection(const Value: Boolean);
  public
    constructor Create(const UseEagerRuleSelection: Boolean = True);
    procedure AddRule(ReplacePattern, ReplaceValue: string; const GlobalFilterPattern: string = ''); overload;
    procedure AddRule(const ReplacePattern: string; const ReplaceValueList: IEnumerable<string>;
      const GlobalFilterPattern: string = ''); overload;
    procedure Clear; reintroduce;
    function Transform(const Source: string): IEnumerable<string>; overload;
    function Transform(const Source: string; GetReplaceString: TFunc<string, string>): IEnumerable<string>; overload;
    property UseEagerRuleSelection: Boolean read GetUseEagerRuleSelection write SetUseEagerRuleSelection;
  end;

implementation

uses
  DSharp.Core.RegularExpressions;

{ TRule }

constructor TRule.Create(const ReplacePattern: string; const ReplacementValues: IEnumerable<string>;
  const GlobalFilterPattern: string);
begin
  FReplacePattern := ReplacePattern;
  FReplacementValues := ReplacementValues;
  FGlobalFilterPattern := GlobalFilterPattern;
end;

procedure TNameTransformer.Clear;
begin
  inherited Clear;
end;

constructor TNameTransformer.Create(const UseEagerRuleSelection: Boolean = True);
begin
  inherited Create;
  FUseEagerRuleSelection := UseEagerRuleSelection;
end;

{ TNameTransformer }

procedure TNameTransformer.AddRule(ReplacePattern, ReplaceValue: string; const GlobalFilterPattern: string);
begin
  AddRule(ReplacePattern, TList<string>.Create(ReplaceValue), GlobalFilterPattern);
end;

procedure TNameTransformer.AddRule(const ReplacePattern: string; const ReplaceValueList: IEnumerable<string>;
  const GlobalFilterPattern: string = '');
begin
  Add(TRule.Create(ReplacePattern, ReplaceValueList, GlobalFilterPattern));
end;

function TNameTransformer.GetUseEagerRuleSelection: Boolean;
begin
  Result := FUseEagerRuleSelection;
end;

procedure TNameTransformer.SetUseEagerRuleSelection(const Value: Boolean);
begin
  FUseEagerRuleSelection := Value;
end;

function TNameTransformer.Transform(const Source: string): IEnumerable<string>;
begin
  Result := Transform(Source,
    function(Value: string): string
    begin
      Result := Value;
    end);
end;

function TNameTransformer.Transform(const Source: string; GetReplaceString: TFunc<string, string>): IEnumerable<string>;
var
  i: Integer;
  LNameList: IList<string>;
  LReplacementValue: string;
  LRepString: string;
  LRules: IList<TRule>;
  LRule: TRule;
begin
  LNameList := TList<string>.Create;

  // Reverse
  LRules:= TList<TRule>.Create;
  for i := Count - 1 downto 0 do
  begin
    LRules.Add(Items[i]);
  end;

  for LRule in LRules do
  begin
    if (LRule.GlobalFilterPattern <> EmptyStr) and (not TRegEx.IsMatch(Source, LRule.GlobalFilterPattern)) then
    begin
      Continue;
    end;

    if not TRegEx.IsMatch(Source, LRule.ReplacePattern) then
    begin
      Continue;
    end;

    for LReplacementValue in LRule.ReplacementValues do
    begin
      LRepString := GetReplaceString(LReplacementValue);
      LNameList.Add(TRegEx.Replace(Source, LRule.ReplacePattern, LRepString));
    end;

    if not UseEagerRuleSelection then
    begin
      Break;
    end;
  end;

  Result := LNameList;
end;

end.
