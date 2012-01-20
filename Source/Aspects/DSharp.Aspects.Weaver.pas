(*
  Copyright (c) 2011, Stefan Glienke
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

unit DSharp.Aspects.Weaver;

interface

uses
  DSharp.Aspects,
  DSharp.Aspects.Intercepts,
  Generics.Collections,
  Rtti,
  TypInfo;

type
  AspectWeaver = record
  private
    class var FIntercepts: TObjectDictionary<TRttiType, TIntercept>;
    class function GetIntercept(RttiType: TRttiType): TIntercept; static;
    class procedure InternalAddAspect(RttiType: TRttiType;
      AspectClass: TAspectClass; const MethodName: string); static;

    class procedure InitializeAspects; static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure AddAspect(AClass: TClass; AAspectClass: TAspectClass;
      const AMethodName: string; AIncludeDerivedTypes: Boolean = True); overload; static;
    class procedure AddAspect(AGuid: TGUID; AAspectClass: TAspectClass;
      const AMethodName: string; AIncludeDerivedTypes: Boolean = True); overload; static;
    class procedure AddAspect(ARttiType: TRttiType; AAspectClass: TAspectClass;
      const AMethodName: string; AIncludeDerivedTypes: Boolean = True); overload; static;

    class function Proxify(Instance: IInterface; TypeInfo: PTypeInfo): IInterface; overload; static;
    class function Proxify<T: IInterface>(Instance: T): T; overload; static;
  end;

  TAspectWeaver = class(TInterfacedObject, IAspectWeaver)
  public
    procedure AddAspect(Guid: TGUID; AspectClass: TAspectClass);
    function Proxify(Instance: IInterface; TypeInfo: PTypeInfo): IInterface;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Core.RegularExpressions;

{ AspectWeaver }

class constructor AspectWeaver.Create;
begin
  FIntercepts := TObjectDictionary<TRttiType, TIntercept>.Create([doOwnsValues]);
end;

class destructor AspectWeaver.Destroy;
begin
  FIntercepts.Free();
end;

class procedure AspectWeaver.AddAspect(AClass: TClass;
  AAspectClass: TAspectClass; const AMethodName: string;
  AIncludeDerivedTypes: Boolean = True);
var
  LType: TRttiType;
begin
  LType := GetRttiType(AClass);
  AspectWeaver.AddAspect(LType, AAspectClass, AMethodName);
end;

class procedure AspectWeaver.AddAspect(AGuid: TGUID;
  AAspectClass: TAspectClass; const AMethodName: string;
  AIncludeDerivedTypes: Boolean);
var
  LType: TRttiType;
begin
  FindType(AGuid, LType);
  AspectWeaver.AddAspect(LType, AAspectClass, AMethodName);
end;

class procedure AspectWeaver.AddAspect(ARttiType: TRttiType;
  AAspectClass: TAspectClass; const AMethodName: string;
  AIncludeDerivedTypes: Boolean);
var
  LType: TRttiType;
begin
  AspectWeaver.InternalAddAspect(ARttiType, AAspectClass, AMethodName);

  if AIncludeDerivedTypes then
  begin
    for LType in GetRttiTypes do
    begin
      if LType.IsInheritedFrom(ARttiType) and (LType.Handle <> ARttiType.Handle) then
      try
        AspectWeaver.InternalAddAspect(LType, AAspectClass, AMethodName);
      except
      end;
    end;
  end;
end;

class procedure AspectWeaver.InitializeAspects;
var
  LType: TRttiType;
  LAspectAttribute: AspectAttribute;
  LMethod: TRttiMethod;
begin
  for LType in GetRttiTypes do
  begin
    if LType.IsInstance or LType.IsInterface then
    begin
      for LAspectAttribute in LType.GetAttributesOfType<AspectAttribute> do
      begin
        AddAspect(LType, LAspectAttribute.AspectClass, '.*');
      end;

      for LMethod in LType.GetMethods do
      begin
        for LAspectAttribute in LMethod.GetAttributesOfType<AspectAttribute> do
        begin
          AddAspect(LType, LAspectAttribute.AspectClass, LMethod.Name);
        end;
      end;
    end;
  end;
end;

class function AspectWeaver.GetIntercept(RttiType: TRttiType): TIntercept;
begin
  if not FIntercepts.TryGetValue(RttiType, Result) then
  begin
    if RttiType.IsInstance then
    begin
      Result := TClassIntercept.Create(RttiType.AsInstance.MetaclassType);
    end else
    if RttiType.IsInterface then
    begin
      Result := TInterfaceIntercept.Create(RttiType.Handle);
    end;
    FIntercepts.Add(RttiType, Result);
  end;
end;

class procedure AspectWeaver.InternalAddAspect(RttiType: TRttiType;
  AspectClass: TAspectClass; const MethodName: string);
var
  LIntercept: TIntercept;
  LMethod: TRttiMethod;
begin
  LIntercept := AspectWeaver.GetIntercept(RttiType);

  for LMethod in RttiType.GetMethods do
  begin
    if (LMethod.VirtualIndex >= 0) and TRegEx.IsMatch(LMethod.Name, MethodName) then
    begin
      LIntercept.Add(LMethod, AspectClass);
    end;
  end;
end;

class function AspectWeaver.Proxify(Instance: IInterface; TypeInfo: PTypeInfo): IInterface;
var
  LType: TRttiType;
  LIntercept: TIntercept;
begin
  if TryGetRttiType(TypeInfo, LType) then
  begin
    LIntercept := AspectWeaver.GetIntercept(LType);
    Result := (LIntercept as TInterfaceIntercept).Proxify(Instance);
  end else
  begin
    Result := Instance;
  end;
end;

class function AspectWeaver.Proxify<T>(Instance: T): T;
begin
  Result := T(AspectWeaver.Proxify(Instance, TypeInfo(T)));
end;

{ TAspectWeaver }

procedure TAspectWeaver.AddAspect(Guid: TGUID; AspectClass: TAspectClass);
begin
  AspectWeaver.AddAspect(Guid, AspectClass, '.*');
end;

function TAspectWeaver.Proxify(Instance: IInterface; TypeInfo: PTypeInfo): IInterface;
begin
  Result := AspectWeaver.Proxify(Instance, TypeInfo)
end;

initialization
  AspectWeaver.InitializeAspects;

end.
