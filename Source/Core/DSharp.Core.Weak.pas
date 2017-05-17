(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Core.Weak;

interface

uses
  Generics.Collections;

type
  IWeak = interface
    function GetIsAlive: Boolean;
    function GetTarget: IInterface;
    property IsAlive: Boolean read GetIsAlive;
    property Target: IInterface read GetTarget;
  end;

  TWeak = class(TInterfacedObject, IWeak)
  private
    FInstance: TObject;
    FReference: Pointer;
    function GetIsAlive: Boolean;
    function GetTarget: IInterface;
    constructor Create(const Reference: IInterface);
  public
    destructor Destroy; override;
    class function Get(const Reference: IInterface): IWeak;

    property IsAlive: Boolean read GetIsAlive;
  end;

  Weak<T: IInterface> = record
  private
    FWeak: IWeak;
    function GetIsAlive: Boolean;
    function GetTarget: T;
    procedure SetTarget(const Value: T);
  public
    class operator Implicit(const Value: T): Weak<T>; inline;
    class operator Implicit(const Value: Weak<T>): T; inline;
    property IsAlive: Boolean read GetIsAlive;
    property Target: T read GetTarget write SetTarget;
  end;

implementation

uses
  DSharp.Core.Detour,
  SyncObjs;

type
  TMethodCall = procedure(Self: TObject);

var
  Hooks: TDictionary<TClass, TMethodCall>;
  Lock: TCriticalSection;
  References: TDictionary<TObject, IWeak>;

procedure FreeInstance(Self: TObject);
var
  LFreeInstance: TMethodCall;
begin
  Lock.Enter;
  try
    References.Remove(Self);
    LFreeInstance := Hooks.Items[Self.ClassType];
  finally
    Lock.Leave;
  end;

  LFreeInstance(Self);
end;

procedure HookFreeInstance(Instance: TObject);
begin
  Lock.Enter;
  try
    if not Hooks.ContainsKey(Instance.ClassType) then
    begin
{$WARN SYMBOL_DEPRECATED OFF}
      Hooks.Add(Instance.ClassType, PPointer(NativeInt(Instance.ClassType) + vmtFreeInstance)^);
      PatchCode(Pointer(NativeInt(Instance.ClassType) + vmtFreeInstance), @FreeInstance);
{$WARN SYMBOL_DEPRECATED ON}
    end;
  finally
    Lock.Leave;
  end;
end;

{ TWeak }

constructor TWeak.Create(const Reference: IInterface);
begin
  FInstance := Reference as TObject;
  FReference := Pointer(Reference);

  Lock.Enter;
  try
    References.Add(FInstance, Self);
  finally
    Lock.Leave;
  end;

  HookFreeInstance(FInstance);
end;

destructor TWeak.Destroy;
begin
  Lock.Enter;
  try
    References.Remove(FInstance);
  finally
    Lock.Leave;
  end;
  inherited;
end;

class function TWeak.Get(const Reference: IInterface): IWeak;
begin
  if Assigned(Reference) then
  begin
    Lock.Enter;
    try
      if not References.TryGetValue(Reference as TObject, Result) then
        Result := TWeak.Create(Reference);
    finally
      Lock.Leave;
    end;
  end;
end;

function TWeak.GetIsAlive: Boolean;
begin
  Lock.Enter;
  try
    Result := References.ContainsKey(FInstance);
  finally
    Lock.Leave;
  end;
end;

function TWeak.GetTarget: IInterface;
begin
  if IsAlive then
    Result := IInterface(FReference)
  else
    Result := nil;
end;

{ Weak<T> }

function Weak<T>.GetIsAlive: Boolean;
begin
  if Assigned(FWeak) and not FWeak.IsAlive then
    FWeak := nil;
  Result := Assigned(FWeak);
end;

function Weak<T>.GetTarget: T;
begin
  if IsAlive then
    Result := T(FWeak.GetTarget)
  else
    Result := Default(T);
end;

procedure Weak<T>.SetTarget(const Value: T);
begin
  FWeak := TWeak.Get(IInterface(Value));
end;

class operator Weak<T>.Implicit(const Value: T): Weak<T>;
begin
  Result.Target := Value;
end;

class operator Weak<T>.Implicit(const Value: Weak<T>): T;
begin
  Result := Value.Target;
end;

initialization
  Lock := TCriticalSection.Create;
  Hooks := TDictionary<TClass, TMethodCall>.Create;
  References := TDictionary<TObject, IWeak>.Create;

finalization
  References.Free;
  Hooks.Free;
  Lock.Free;

end.
