unit DSharp.Interception.InjectionPolicy;

interface

uses
  DSharp.Interception,
  DSharp.Interception.MethodImplementationInfo,
  Rtti;

type
  TInjectionPolicy = class(TInterfacedObject, IInjectionPolicy)
  private
    FName: string;
    FDoesNotHaveNoPoliciesAttributeRule: IMatchingRule;
    function GetName: string;
  protected
    function DoesMatch(Member: TMethodImplementationInfo): Boolean; virtual; abstract;
    function DoGetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>; virtual; abstract;
  public
    constructor Create; overload;
    constructor Create(const Name: string); overload;

    function GetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>; virtual;
    function Matches(Member: TMethodImplementationInfo): Boolean;

    property Name: string read GetName;
  end;

implementation

uses
  DSharp.Interception.ApplyNoPoliciesMatchingRule;

{ TInjectionPolicy }

constructor TInjectionPolicy.Create;
begin
  Create('Unnamed policy');
end;

constructor TInjectionPolicy.Create(const Name: string);
begin
  FName := Name;
  FDoesNotHaveNoPoliciesAttributeRule := TApplyNoPoliciesMatchingRule.Create;
end;

function TInjectionPolicy.GetHandlersFor(
  Member: TMethodImplementationInfo): TArray<ICallHandler>;
begin
  Result := DoGetHandlersFor(Member);
end;

function TInjectionPolicy.GetName: string;
begin
  Result := FName;
end;

function TInjectionPolicy.Matches(Member: TMethodImplementationInfo): Boolean;
begin
  Result := DoesMatch(Member);
end;

end.
