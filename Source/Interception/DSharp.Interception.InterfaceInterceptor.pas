(*
  Copyright (c) 2012-2014, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

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
