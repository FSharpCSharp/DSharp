unit DSharp.Interception.MethodReturn;

interface

uses
  DSharp.Interception,
  Rtti,
  SysUtils;

type
  TMethodReturn = class(TInterfacedObject, IMethodReturn)
  private
    FException: Exception;
    FReturnValue: TValue;
    function GetException: Exception;
    function GetReturnValue: TValue;
    procedure SetException(const Value: Exception);
    procedure SetReturnValue(const Value: TValue);
  public
    constructor Create(Method: TRttiMethod);

    property Exception: Exception read GetException write SetException;
    property ReturnValue: TValue read GetReturnValue write SetReturnValue;
  end;

implementation

{ TMethodReturn }

constructor TMethodReturn.Create(Method: TRttiMethod);
begin
  if Assigned(Method) and Assigned(Method.ReturnType) then
  begin
    TValue.Make(nil, Method.ReturnType.Handle, FReturnValue);
  end;
end;

function TMethodReturn.GetException: Exception;
begin
  Result := FException;
end;

function TMethodReturn.GetReturnValue: TValue;
begin
  Result := FReturnValue;
end;

procedure TMethodReturn.SetException(const Value: Exception);
begin
  FException := Value;
end;

procedure TMethodReturn.SetReturnValue(const Value: TValue);
begin
  FReturnValue := Value;
end;

end.
