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

unit DSharp.Aspects.Intercepts;

interface

uses
  DSharp.Aspects,
  DSharp.Core.Dynamics,
  Generics.Collections,
  Rtti,
  SysUtils,
  TypInfo;

type
  TIntercept = class
  private
    FAspects: TDictionary<TRttiMethod, TList<TAspectClass>>;
    FTypeInfo: PTypeInfo;
  protected
    procedure DoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue);
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoException(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out RaiseException: Boolean;
      Exception: Exception; out Result: TValue);
  public
    constructor Create(TypeInfo: PTypeInfo);
    destructor Destroy; override;

    procedure Add(AMethod: TRttiMethod; AAspectClass: TAspectClass);
    property TypeInfo: PTypeInfo read FTypeInfo;
  end;

  TClassIntercept = class(TIntercept)
  private
    FInterceptor: TVirtualMethodInterceptor;
    FOriginalClassData: Pointer;
    FVirtualMethodCount: Cardinal;
    function GetVirtualMethodCount(AClass: TClass): Cardinal;
    procedure ReplaceOriginalClass;
    procedure RestoreOriginalClass;
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;
  end;

  TInterfaceIntercept = class(TIntercept)
  private
    FInterceptor: TInterfaceMethodInterceptor;
  public
    constructor Create(ATypeInfo: PTypeInfo);
    destructor Destroy; override;

    function Proxify(Instance: IInterface): IInterface;
  end;

implementation

uses
  DSharp.Core.Detour,
  DSharp.Core.Reflection;

{ TIntercept }

constructor TIntercept.Create;
begin
  FAspects := TObjectDictionary<TRttiMethod, TList<TAspectClass>>.Create([doOwnsValues]);
end;

destructor TIntercept.Destroy;
begin
  FAspects.Free();
  inherited;
end;

procedure TIntercept.Add(AMethod: TRttiMethod; AAspectClass: TAspectClass);
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

procedure TIntercept.DoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
var
  i: Integer;
  LAspects: TList<TAspectClass>;
begin
  if FAspects.TryGetValue(Method, LAspects) then
  begin
    for i := 0 to Pred(LAspects.Count) do
    begin
      LAspects[i].DoAfter(Instance, Method, Args, Result);
    end;
  end;
end;

procedure TIntercept.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
var
  i: Integer;
  LAspects: TList<TAspectClass>;
begin
  if FAspects.TryGetValue(Method, LAspects) then
  begin
    for i := 0 to Pred(LAspects.Count) do
    begin
      LAspects[i].DoBefore(Instance, Method, Args, DoInvoke, Result);
    end;
  end;
end;

procedure TIntercept.DoException(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out RaiseException: Boolean; Exception: Exception;
  out Result: TValue);
var
  i: Integer;
  LAspects: TList<TAspectClass>;
begin
  if FAspects.TryGetValue(Method, LAspects) then
  begin
    for i := 0 to Pred(LAspects.Count) do
    begin
      LAspects[i].DoException(Instance, Method, Args, RaiseException, Exception, Result);
    end;
  end;
end;

{ TClassAspect }

constructor TClassIntercept.Create(AClass: TClass);
begin
  inherited Create(AClass.ClassInfo);
  FInterceptor := TVirtualMethodInterceptor.Create(AClass);
  FInterceptor.OnBefore := DoBefore;
  FInterceptor.OnAfter := DoAfter;
  FInterceptor.OnException := DoException;
  FVirtualMethodCount := GetVirtualMethodCount(AClass);
  if FVirtualMethodCount > 0 then
    ReplaceOriginalClass();
end;

destructor TClassIntercept.Destroy;
begin
  RestoreOriginalClass();
  FInterceptor.Free();
  inherited;
end;

function TClassIntercept.GetVirtualMethodCount(AClass: TClass): Cardinal;
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

procedure TClassIntercept.ReplaceOriginalClass;
begin
  if FOriginalClassData = nil then
  begin
    FOriginalClassData := AllocMem(FVirtualMethodCount * SizeOf(Pointer));
    WriteMem(FOriginalClassData, FInterceptor.OriginalClass, FVirtualMethodCount * SizeOf(Pointer));
    WriteMem(FInterceptor.OriginalClass, FInterceptor.ProxyClass, FVirtualMethodCount * SizeOf(Pointer));
  end;
end;

procedure TClassIntercept.RestoreOriginalClass;
begin
  if FOriginalClassData <> nil then
  begin
    WriteMem(FInterceptor.OriginalClass, FOriginalClassData, FVirtualMethodCount * SizeOf(Pointer));
    FreeMem(FOriginalClassData);
    FOriginalClassData := nil;
  end;
end;

{ TInterfaceIntercept }

constructor TInterfaceIntercept.Create(ATypeInfo: PTypeInfo);
begin
  inherited;
  FInterceptor := TInterfaceMethodInterceptor.Create(ATypeInfo);
  FInterceptor.OnBefore := DoBefore;
  FInterceptor.OnAfter := DoAfter;
  FInterceptor.OnException := DoException;
end;

destructor TInterfaceIntercept.Destroy;
begin
  FInterceptor.Free;
  inherited;
end;

function TInterfaceIntercept.Proxify(Instance: IInterface): IInterface;
begin
  Result := FInterceptor.Proxify(Instance);
end;

end.
