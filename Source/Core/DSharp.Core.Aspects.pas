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

unit DSharp.Core.Aspects;

interface

uses
  Generics.Collections,
  Rtti,
  SysUtils;

type
  TAspect = class(TCustomAttribute)
  protected
    class procedure DoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue); virtual;
    class procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean;
      out Result: TValue); virtual;
    class procedure DoException(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out RaiseException: Boolean;
      Exception: Exception; out Result: TValue); virtual;
  end;

  TAspectClass = class of TAspect;

  TClassWeaver = class
  private
    FAspects: TDictionary<TRttiMethod, TList<TAspectClass>>;
    FInterceptor: TVirtualMethodInterceptor;
    FOriginalClassData: Pointer;
    FVirtualMethodCount: Cardinal;
    procedure DoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue);
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoException(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out RaiseException: Boolean;
      Exception: Exception; out Result: TValue);
    function GetVirtualMethodCount(AClass: TClass): Cardinal;
    procedure ReplaceOriginalClass;
    procedure RestoreOriginalClass;
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;

    procedure Add(AMethod: TRttiMethod; AAspectClass: TAspectClass);
  end;

  AspectWeaver = record
  private
    class var FClassWeavers: TObjectDictionary<TClass, TClassWeaver>;
    class procedure InternalAddAspect(AClass: TClass;
      AAspectClass: TAspectClass; const AMethodName: string); static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure AddAspect(AClass: TClass; AAspectClass: TAspectClass;
      const AMethodName: string; AIncludeDerivedClasses: Boolean = True); static;
  end;

implementation

uses
  DSharp.Core.Reflection,
  RegularExpressions,
  Windows;

resourcestring
  RMemoryWriteError = 'Error writing memory (%s)';

function WriteProtectedMemory(BaseAddress, Buffer: Pointer;
  Size: Cardinal; out WrittenBytes: Cardinal): Boolean;
begin
  Result := WriteProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, WrittenBytes);
end;

procedure WriteMem(const Location, Buffer: Pointer; const Size: Cardinal);
var
  WrittenBytes: Cardinal;
  SaveFlag: Cardinal;
begin
  if VirtualProtect(Location, Size, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    if not WriteProtectedMemory(Location, Buffer, Size, WrittenBytes) then
      raise Exception.CreateResFmt(@RMemoryWriteError, [SysErrorMessage(GetLastError)]);
  finally
    VirtualProtect(Location, Size, SaveFlag, @SaveFlag);
  end;

  if WrittenBytes <> Size then
    raise Exception.CreateResFmt(@RMemoryWriteError, [IntToStr(WrittenBytes)]);

  FlushInstructionCache(GetCurrentProcess, Location, Size);
end;

{ TClassAspect }

constructor TClassWeaver.Create(AClass: TClass);
begin
  FAspects := TObjectDictionary<TRttiMethod, TList<TAspectClass>>.Create([doOwnsValues]);
  FInterceptor := TVirtualMethodInterceptor.Create(AClass);
  FInterceptor.OnBefore := DoBefore;
  FInterceptor.OnAfter := DoAfter;
  FInterceptor.OnException := DoException;
  FVirtualMethodCount := GetVirtualMethodCount(AClass);
  ReplaceOriginalClass();
end;

destructor TClassWeaver.Destroy;
begin
  RestoreOriginalClass();
  FInterceptor.Free();
  FAspects.Free();
  inherited;
end;

procedure TClassWeaver.DoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
var
  LAspect: TAspectClass;
begin
  if FAspects.ContainsKey(Method) then
  begin
    for LAspect in FAspects.Items[Method] do
    begin
      LAspect.DoAfter(Instance, Method, Args, Result);
    end;
  end;
end;

procedure TClassWeaver.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
var
  LAspect: TAspectClass;
begin
  if FAspects.ContainsKey(Method) then
  begin
    for LAspect in FAspects.Items[Method] do
    begin
      LAspect.DoBefore(Instance, Method, Args, DoInvoke, Result);
    end;
  end;
end;

procedure TClassWeaver.DoException(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out RaiseException: Boolean; Exception: Exception;
  out Result: TValue);
var
  LAspect: TAspectClass;
begin
  if FAspects.ContainsKey(Method) then
  begin
    for LAspect in FAspects.Items[Method] do
    begin
      LAspect.DoException(Instance, Method, Args, RaiseException, Exception, Result);
    end;
  end;
end;

procedure TClassWeaver.Add(AMethod: TRttiMethod; AAspectClass: TAspectClass);
var
  LAspects: TList<TAspectClass>;
begin
  if not FAspects.TryGetValue(AMethod, LAspects) then
  begin
    LAspects := TList<TAspectClass>.Create();
    FAspects.Add(AMethod, LAspects);
  end;
  if not LAspects.Contains(AAspectClass) then
  begin
    LAspects.Add(AAspectClass);
  end;
end;

function TClassWeaver.GetVirtualMethodCount(AClass: TClass): Cardinal;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LMaxIndex: Integer;
begin
  LType := GetRttiType(AClass);
  LMaxIndex := -1;
  for LMethod in LType.GetMethods do
  begin
    if (LMethod.DispatchKind = dkVtable) and (LMethod.VirtualIndex > LMaxIndex) then
    begin
      LMaxIndex := LMethod.VirtualIndex;
    end;
  end;
  Result := LMaxIndex + 1;
end;

procedure TClassWeaver.ReplaceOriginalClass;
begin
  if FOriginalClassData = nil then
  begin
    FOriginalClassData := AllocMem(FVirtualMethodCount * SizeOf(Pointer));
    WriteMem(FOriginalClassData, FInterceptor.OriginalClass, FVirtualMethodCount * SizeOf(Pointer));
    WriteMem(FInterceptor.OriginalClass, FInterceptor.ProxyClass, FVirtualMethodCount * SizeOf(Pointer));
  end;
end;

procedure TClassWeaver.RestoreOriginalClass;
begin
  if FOriginalClassData <> nil then
  begin
    WriteMem(FInterceptor.OriginalClass, FOriginalClassData, FVirtualMethodCount * SizeOf(Pointer));
    FreeMem(FOriginalClassData);
    FOriginalClassData := nil;
  end;
end;

{ TAspect }

class procedure TAspect.DoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
begin
  // implemented by descendants
end;

class procedure TAspect.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
begin
  // implemented by descendants
end;

class procedure TAspect.DoException(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out RaiseException: Boolean; Exception: Exception;
  out Result: TValue);
begin
  // implemented by descendants
end;

{ AspectWeaver }

class constructor AspectWeaver.Create;
begin
  FClassWeavers := TObjectDictionary<TClass, TClassWeaver>.Create([doOwnsValues]);
end;

class destructor AspectWeaver.Destroy;
begin
  FClassWeavers.Free();
end;

class procedure AspectWeaver.InternalAddAspect(AClass: TClass;
  AAspectClass: TAspectClass; const AMethodName: string);
var
  LClassWeaver: TClassWeaver;
  LType: TRttiType;
  LMethod: TRttiMethod;
begin
  if not FClassWeavers.TryGetValue(AClass, LClassWeaver) then
  begin
    LClassWeaver := TClassWeaver.Create(AClass);
    FClassWeavers.Add(AClass, LClassWeaver);
  end;
  if TryGetRttiType(AClass, LType) then
  begin
    for LMethod in LType.GetMethods do
    begin
      if (LMethod.VirtualIndex >= 0) and TRegEx.IsMatch(LMethod.Name, AMethodName, []) then
      begin
        LClassWeaver.Add(LMethod, AAspectClass);
      end;
    end;
  end;
end;

class procedure AspectWeaver.AddAspect(AClass: TClass;
  AAspectClass: TAspectClass; const AMethodName: string;
  AIncludeDerivedClasses: Boolean = True);
var
  LType: TRttiType;
begin
  AspectWeaver.InternalAddAspect(AClass, AAspectClass, AMethodName);

  if AIncludeDerivedClasses then
  begin
    for LType in GetRttiTypes do
    begin
      if LType.IsInstance and LType.AsInstance.MetaclassType.InheritsFrom(AClass)
        and (LType.AsInstance.MetaclassType <> AClass) then
      try
        AspectWeaver.InternalAddAspect(
          LType.AsInstance.MetaclassType, AAspectClass, AMethodName);
      except
      end;
    end;
  end;
end;

end.
