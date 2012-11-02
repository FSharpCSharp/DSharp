unit DSharp.Interception.MethodInvocation;

interface

uses
  DSharp.Interception,
  Rtti,
  SysUtils;

type
  TMethodInvocation = class(TInterfacedObject, IMethodInvocation)
  private
    FArguments: TArray<TValue>;
    FMethod: TRttiMethod;
    FTarget: TValue;
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetTarget: TValue;
  public
    constructor Create(Method: TRttiMethod; Target: TValue; const Arguments: TArray<TValue>);

    function CreateMethodReturn(const ReturnValue: TValue): IMethodReturn;
    function CreateExceptionMethodReturn(E: Exception): IMethodReturn;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Target: TValue read GetTarget;
  end;

implementation

uses
  DSharp.Interception.MethodReturn;

{ TMethodInvocation }

constructor TMethodInvocation.Create(Method: TRttiMethod; Target: TValue;
  const Arguments: TArray<TValue>);
begin
  FMethod := Method;
  FTarget := Target;
  FArguments := Arguments;
end;

function TMethodInvocation.CreateExceptionMethodReturn(
  E: Exception): IMethodReturn;
begin
  Result := TMethodReturn.Create(FMethod);
  Result.Exception := E;
end;

function TMethodInvocation.CreateMethodReturn(
  const ReturnValue: TValue): IMethodReturn;
begin
  Result := TMethodReturn.Create(FMethod);
  Result.ReturnValue := ReturnValue;
end;

function TMethodInvocation.GetArguments: TArray<TValue>;
begin
  Result := FArguments;
end;

function TMethodInvocation.GetMethod: TRttiMethod;
begin
  Result := FMethod;
end;

function TMethodInvocation.GetTarget: TValue;
begin
  Result := FTarget;
end;

end.
