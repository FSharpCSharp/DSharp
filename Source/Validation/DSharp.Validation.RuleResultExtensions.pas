unit DSharp.Validation.RuleResultExtensions;

interface

uses
  SysUtils,
  DSharp.Validation.RuleResult;

type
  ///	<summary>
  ///	  Contains helper extension methods for working with
  ///	  <see cref="RuleResult" />.
  ///	</summary>
  TRuleResultExtensions = record helper for TRuleResult
  public
    ///	<summary>
    ///	  Merges <paramref name="RuleResult" /> with given
    ///	  <paramref name="SecondRuleResult" /> and returns a new instance of
    ///	  <see cref="TRuleResult" /> that represents the merged result (the
    ///	  result that contains errors from both results without duplicates).
    ///	</summary>
    ///	<param name="SecondRuleResult">
    ///	  The second validation result to merge.
    ///	</param>
    ///	<returns>
    ///	  A new instance of <see cref="TRuleResult" /> that represents the
    ///	  merged result (the result that contains errors from both results
    ///	  whithout duplicates).
    ///	</returns>
    function Combine(const SecondRuleResult: TRuleResult): TRuleResult;
  end;

implementation

uses
  StrUtils;

{ TRuleResultExtensions }

function TRuleResultExtensions.Combine(const SecondRuleResult: TRuleResult)
  : TRuleResult;
var
  LError: string;
begin
  Result := TRuleResult.Valid;

  for LError in Self.Errors do
  begin
    Result.AddError(LError);
  end;

  for LError in SecondRuleResult.Errors do
  begin
    if IndexStr(LError, Result.Errors) = -1 then
    begin
      Result.AddError(LError);
    end;
  end;
end;

end.
