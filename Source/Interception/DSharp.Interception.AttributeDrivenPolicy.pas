unit DSharp.Interception.AttributeDrivenPolicy;

interface

uses
  DSharp.Interception,
  DSharp.Interception.InjectionPolicy,
  DSharp.Interception.MethodImplementationInfo,
  Rtti;

type
  TAttributeDrivenPolicy = class(TInjectionPolicy)
  private
    FAttributeMatchingRule: IMatchingRule;
  protected
    function DoesMatch(Member: TMethodImplementationInfo): Boolean; override;
    function DoGetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>; override;
  public
    constructor Create;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Core.Utils,
  DSharp.Interception.AttributeDrivenPolicyMatchingRule,
  Math;

{ TAttributeDrivenPolicy }

constructor TAttributeDrivenPolicy.Create;
begin
  inherited Create('Attribute Driven Policy');
  FAttributeMatchingRule := TAttributeDrivenPolicyMatchingRule.Create;
end;

function TAttributeDrivenPolicy.DoesMatch(Member: TMethodImplementationInfo): Boolean;
var
  matchesInterface: Boolean;
  matchesImplementation: Boolean;
begin
  matchesInterface := IfThen(Assigned(Member.InterfaceMethodInfo),
    FAttributeMatchingRule.Matches(Member.InterfaceMethodInfo));
  matchesImplementation := FAttributeMatchingRule.Matches(Member.ImplementationMethodInfo);

  Result := matchesInterface or matchesImplementation;
end;

function TAttributeDrivenPolicy.DoGetHandlersFor(
  Member: TMethodImplementationInfo): TArray<ICallHandler>;
var
  attribute: HandlerAttribute;
begin
  if Assigned(Member.InterfaceMethodInfo) then
  begin
    for attribute in Member.InterfaceMethodInfo.GetCustomAttributes<HandlerAttribute>(True) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute.CreateHandler;
    end;
  end;
  if Assigned(Member.ImplementationMethodInfo) then
  begin
    for attribute in Member.ImplementationMethodInfo.GetCustomAttributes<HandlerAttribute>(True) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute.CreateHandler;
    end;
  end;
end;

end.
