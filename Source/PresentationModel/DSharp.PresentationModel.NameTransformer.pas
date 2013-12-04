unit DSharp.PresentationModel.NameTransformer;

interface

uses
  DSharp.Collections;

type
  TRule = class
  private
    FReplacePattern: string;
    FReplacementValue: string;
  public
    constructor Create(const ReplacePattern, ReplacementValue: string);
    property ReplacePattern: string read FReplacePattern;
    property ReplacementValue: string read FReplacementValue;
  end;

  INameTransformer = interface
    procedure AddRule(const ReplacePattern, ReplacementValue: string);
    function Transform(const Source: string): IEnumerable<string>;
  end;

  TNameTransformer = class(TObjectList<TRule>, INameTransformer)
  public
    procedure AddRule(const ReplacePattern, ReplacementValue: string);
    function Transform(const Source: string): IEnumerable<string>;
  end;

implementation

uses
  DSharp.Core.RegularExpressions;

{ TRule }

constructor TRule.Create(const ReplacePattern, ReplacementValue: string);
begin
  FReplacePattern := ReplacePattern;
  FReplacementValue := ReplacementValue
end;

{ TNameTransformer }

procedure TNameTransformer.AddRule(const ReplacePattern,
  ReplacementValue: string);
begin
  Add(TRule.Create(ReplacePattern, ReplacementValue));
end;

function TNameTransformer.Transform(const Source: string): IEnumerable<string>;
var
  LNameList: TList<string>;
  LRule: TRule;
begin
  LNameList := TList<string>.Create;

  for LRule in Self do
  begin
    if TRegEx.IsMatch(Source, LRule.ReplacePattern) then
    begin
      LNameList.Add(TRegEx.Replace(Source, LRule.ReplacePattern,
        LRule.ReplacementValue));
    end;
  end;

  Result := LNameList;
end;

end.
