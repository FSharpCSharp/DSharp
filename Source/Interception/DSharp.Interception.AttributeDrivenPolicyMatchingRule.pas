unit DSharp.Interception.AttributeDrivenPolicyMatchingRule;

interface

uses
  DSharp.Interception,
  Rtti;

type
  TAttributeDrivenPolicyMatchingRule = class(TInterfacedObject, IMatchingRule)
  public
    function Matches(Member: TRttiMethod): Boolean;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ TAttributeDrivenPolicyMatchingRule }

function TAttributeDrivenPolicyMatchingRule.Matches(
  Member: TRttiMethod): Boolean;
begin
  Result := Member.IsDefined<HandlerAttribute>(True);
end;

end.
