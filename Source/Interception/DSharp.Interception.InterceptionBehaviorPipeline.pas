unit DSharp.Interception.InterceptionBehaviorPipeline;

interface

uses
  DSharp.Interception,
  Generics.Collections;

type
  TInterceptionBehaviorPipeline = class
  private
    FInterceptionBehaviors: TList<IInterceptionBehavior>;
    function GetCount: Integer;
  public
    constructor Create; overload;
    constructor Create(InterceptionBehaviors: array of IInterceptionBehavior); overload;
    destructor Destroy; override;

    procedure Add(InterceptionBehavior: IInterceptionBehavior);
    function Invoke(Input: IMethodInvocation; Target: TInvokeInterceptionBehaviorDelegate): IMethodReturn;

    property Count: Integer read GetCount;
  end;

implementation

{ TInterceptionBehaviorPipeline }

constructor TInterceptionBehaviorPipeline.Create;
begin
  FInterceptionBehaviors := TList<IInterceptionBehavior>.Create;
end;

constructor TInterceptionBehaviorPipeline.Create(
  InterceptionBehaviors: array of IInterceptionBehavior);
begin
  Create;
  FInterceptionBehaviors.AddRange(InterceptionBehaviors);
end;

destructor TInterceptionBehaviorPipeline.Destroy;
begin
  FInterceptionBehaviors.Free;
  inherited;
end;

function TInterceptionBehaviorPipeline.GetCount: Integer;
begin
  Result := FInterceptionBehaviors.Count;
end;

procedure TInterceptionBehaviorPipeline.Add(
  InterceptionBehavior: IInterceptionBehavior);
begin
  FInterceptionBehaviors.Add(InterceptionBehavior);
end;

function TInterceptionBehaviorPipeline.Invoke(Input: IMethodInvocation;
  Target: TInvokeInterceptionBehaviorDelegate): IMethodReturn;
var
  interceptorIndex: Integer;
begin
  if FInterceptionBehaviors.Count = 0 then
  begin
    Result := Target(Input, nil);
  end
  else
  begin
    interceptorIndex := 0;

    Result := FInterceptionBehaviors[0].Invoke(Input,
      function: TInvokeInterceptionBehaviorDelegate
      begin
        Inc(interceptorIndex);
        if interceptorIndex < FInterceptionBehaviors.Count then
        begin
          Result := FInterceptionBehaviors[interceptorIndex] as IInvokeInterceptionBehaviorDelegate;
        end
        else
        begin
          Result := Target;
        end;
      end);
  end;
end;

end.
