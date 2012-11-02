unit DSharp.Interception.ApplyNoPoliciesMatchingRule;

interface

uses
  DSharp.Interception,
  Rtti;

type
  TApplyNoPoliciesMatchingRule = class(TInterfacedObject, IMatchingRule)
  public
    function Matches(Member: TRttiMethod): Boolean;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ TApplyNoPoliciesMatchingRule }

function TApplyNoPoliciesMatchingRule.Matches(Member: TRttiMethod): Boolean;
begin
  Result := Member.IsDefined<ApplyNoPoliciesAttribute>
    or Member.Parent.IsDefined<ApplyNoPoliciesAttribute>;
end;

end.
