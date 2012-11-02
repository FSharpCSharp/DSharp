unit DSharp.Interception.HandlerPipeline;

interface

uses
  DSharp.Interception,
  Generics.Collections;

type
  THandlerPipeline = class
  private
    FHandlers: TList<ICallHandler>;
    function GetCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const Handlers: array of ICallHandler); overload;
    destructor Destroy; override;

    function Invoke(Input: IMethodInvocation; Target: TInvokeHandlerDelegate): IMethodReturn;

    property Count: Integer read GetCount;
  end;

implementation

{ THandlerPipeline }

constructor THandlerPipeline.Create;
begin
  FHandlers := TList<ICallHandler>.Create;
end;

constructor THandlerPipeline.Create(const Handlers: array of ICallHandler);
begin
  FHandlers := TList<ICallHandler>.Create;
  FHandlers.AddRange(Handlers);
end;

destructor THandlerPipeline.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

function THandlerPipeline.GetCount: Integer;
begin
  Result := FHandlers.Count;
end;

function THandlerPipeline.Invoke(Input: IMethodInvocation;
  Target: TInvokeHandlerDelegate): IMethodReturn;
var
  handlerIndex: Integer;
begin
  if FHandlers.Count = 0 then
  begin
    Result := Target(input, nil);
  end
  else
  begin
    handlerIndex := 0;

    Result := FHandlers[0].Invoke(input,
      function: TInvokeHandlerDelegate
      begin
        Inc(handlerIndex);
        if handlerIndex < FHandlers.Count then
        begin
          Result := FHandlers[handlerIndex] as IInvokeHandlerDelegate;
        end
        else
        begin
          Result := Target;
        end;
      end);
  end;
end;

end.
