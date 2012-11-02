unit DSharp.Interception.PolicySet;

interface

uses
  DSharp.Interception,
  DSharp.Interception.MethodImplementationInfo,
  Generics.Collections,
  Rtti;

type
  TPolicySet = class(TList<IInjectionPolicy>)
  private
    function CalculateHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>;
  public
    constructor Create(const Policies: array of IInjectionPolicy);
    function GetPoliciesFor(Method: TMethodImplementationInfo): TArray<IInjectionPolicy>;
    function GetPoliciesNotFor(Method: TMethodImplementationInfo): TArray<IInjectionPolicy>;
    function GetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>;
  end;

implementation

{ TPolicySet }

constructor TPolicySet.Create(const Policies: array of IInjectionPolicy);
begin
  inherited Create;
  AddRange(Policies);
end;

function TPolicySet.CalculateHandlersFor(
  Member: TMethodImplementationInfo): TArray<ICallHandler>;
var
  ordered: TList<ICallHandler>;
  nonOrdered: TList<ICallHandler>;
  policy: IInjectionPolicy;
  handler: ICallHandler;
  inserted: Boolean;
  i: Integer;
begin
  ordered := TList<ICallHandler>.Create;
  nonOrdered := TList<ICallHandler>.Create;

  try
    for policy in Self do
    begin
      for handler in policy.GetHandlersFor(Member) do
      begin
        if not Assigned(handler) then
          Continue;

        if handler.Order <> 0 then
        begin
          inserted := False;

          for i := ordered.Count - 1 downto 0 do
          begin
            if ordered[i].Order <= handler.Order then
            begin
              ordered.Insert(i + 1, handler);
              inserted := True;
              Break;
            end;
          end;

          if not inserted then
          begin
            ordered.Insert(0, handler);
          end;
        end
        else
        begin
          nonOrdered.Add(handler);
        end;
      end;
    end;

    ordered.AddRange(nonOrdered);
    Result := ordered.ToArray;
  finally
    ordered.Free;
    nonOrdered.Free;
  end;
end;

function TPolicySet.GetHandlersFor(
  Member: TMethodImplementationInfo): TArray<ICallHandler>;
begin
  Result := CalculateHandlersFor(Member);
end;

function TPolicySet.GetPoliciesFor(
  Method: TMethodImplementationInfo): TArray<IInjectionPolicy>;
var
  policy: IInjectionPolicy;
begin
  for policy in Self do
  begin
    if policy.Matches(Method) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := policy;
    end;
  end;
end;

function TPolicySet.GetPoliciesNotFor(
  Method: TMethodImplementationInfo): TArray<IInjectionPolicy>;
var
  policy: IInjectionPolicy;
begin
  for policy in Self do
  begin
    if not policy.Matches(Method) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := policy;
    end;
  end;
end;

end.
