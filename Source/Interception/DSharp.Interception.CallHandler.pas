unit DSharp.Interception.CallHandler;

interface

uses
  DSharp.Interception,
  SysUtils;

type
  TCallHandler = class(TInterfacedObject, ICallHandler, IInvokeHandlerDelegate)
  private
    FOrder: Integer;
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeHandlerDelegate>): IMethodReturn; virtual; abstract;
    property Order: Integer read GetOrder write SetOrder;
  end;

implementation

{ TCallHandler }

function TCallHandler.GetOrder: Integer;
begin
  Result := FOrder;
end;

procedure TCallHandler.SetOrder(const Value: Integer);
begin
  FOrder := Value;
end;

end.
