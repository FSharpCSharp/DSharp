unit DSharp.Interception.ClassInterceptor;

interface

uses
  DSharp.Interception,
  DSharp.Interception.MethodImplementationInfo,
  TypInfo;

type
  TClassInterceptor = class(TInterfacedObject, IInstanceInterceptor, IInterceptor)
  protected
    function CanIntercept(TypeInfo: PTypeInfo): Boolean;
    function GetInterceptableMethods(InterceptedType: PTypeInfo;
      ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
    function CreateProxy(TypeInfo: PTypeInfo; Instance: Pointer): IInterceptingProxy;
  end;

implementation

uses
  DSharp.Interception.ClassProxy;

{ TClassInterceptor }

function TClassInterceptor.CanIntercept(TypeInfo: PTypeInfo): Boolean;
begin
  Result := TypeInfo.Kind = tkClass;
end;

function TClassInterceptor.CreateProxy(TypeInfo: PTypeInfo;
  Instance: Pointer): IInterceptingProxy;
begin
  Result := TClassProxy.Create(TypeInfo, Instance);
end;

function TClassInterceptor.GetInterceptableMethods(InterceptedType,
  ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
begin

end;

end.
