unit DSharp.Interception.InterfaceInterceptor;

interface

uses
  DSharp.Interception,
  DSharp.Interception.MethodImplementationInfo,
  Rtti,
  TypInfo;

type
  TInterfaceInterceptor = class(TInterfacedObject, IInstanceInterceptor, IInterceptor)
  private
    function DoGetInterceptableMethods(InterceptedType: PTypeInfo;
      ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
  public
    function CanIntercept(TypeInfo: PTypeInfo): Boolean;
    function GetInterceptableMethods(InterceptedType: PTypeInfo;
      ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
    function CreateProxy(TypeInfo: PTypeInfo; Instance: Pointer): IInterceptingProxy;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Interception.InterfaceMapping,
  DSharp.Interception.InterfaceProxy;

function IsAssignableFrom(Left, Right: PTypeInfo): Boolean;

  function Implements(AClass: TClass; AInterface: TGUID): Boolean;
  begin
    Result := AClass.GetInterfaceEntry(AInterface) <> nil;

    if not Result and (AClass.ClassParent <> nil) then
    begin
      Result := Implements(AClass.ClassParent, AInterface);
    end;
  end;

begin
  Result := Left = Right;

  if not Result and (Left.Kind = tkClass) then
  begin
    Result := (Right.Kind = tkClass)
      and GetTypeData(Right).ClassType.InheritsFrom(GetTypeData(Left).ClassType);

    if not Result and (Left.Kind = tkInterface) then
    begin
      Result := Implements(GetTypeData(Right).ClassType, GetTypeData(Left).Guid);
    end;
  end;
end;

{ TInterfaceInterceptor }

function TInterfaceInterceptor.CanIntercept(TypeInfo: PTypeInfo): Boolean;
var
  rttiType: TRttiType;
begin
  rttiType := GetRttiType(TypeInfo);
  Result := Assigned(rttiType) and rttiType.IsInterface and (rttiType.MethodCount > 0);
end;

function TInterfaceInterceptor.CreateProxy(TypeInfo: PTypeInfo;
  Instance: Pointer): IInterceptingProxy;
begin
  Result := TInterfaceProxy.Create(TypeInfo, Instance);
end;

function TInterfaceInterceptor.DoGetInterceptableMethods(InterceptedType,
  ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
var
  methods: TArray<TRttiMethod>;
  i: Integer;
  mapping: TInterfaceMapping;
//  rttiType: TRttiType;
begin
  if Assigned(InterceptedType) and (InterceptedType.Kind = tkInterface)
    and Assigned(ImplementationType) and (ImplementationType.Kind = tkInterface)
    and IsAssignableFrom(InterceptedType, ImplementationType) then
  begin
    methods := GetRttiType(InterceptedType).GetMethods;
    SetLength(Result, Length(methods));
    for i := 0 to High(methods) do
    begin
      Result[i] := TMethodImplementationInfo.Create(methods[i], methods[i]);
    end;
  end
  else
  begin
    mapping := TInterfaceMapping.Create(InterceptedType, ImplementationType);
    SetLength(Result, Length(mapping.InterfaceMethods));
    for i := 0 to High(mapping.InterfaceMethods) do
    begin
      Result[i] := TMethodImplementationInfo.Create(
        mapping.InterfaceMethods[i], mapping.TargetMethods[i]);
    end;
  end;
//
//  if GetRttiType(InterceptedType).IsInstance then
//  begin
//    for rttiType in GetRttiType(InterceptedType).AsInstance.GetImplementedInterfaces do
//    begin
//      Result := TArrayHelper.Concat<TMethodImplementationInfo>([Result,
//        DoGetInterceptableMethods(rttiType.Handle, ImplementationType)]);
//    end;
//  end;
end;

function TInterfaceInterceptor.GetInterceptableMethods(InterceptedType,
  ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
begin
  Result := DoGetInterceptableMethods(InterceptedType, ImplementationType);
end;

end.
