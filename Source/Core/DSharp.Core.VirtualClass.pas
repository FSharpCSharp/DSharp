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

unit DSharp.Core.VirtualClass;

interface

{$OVERFLOWCHECKS OFF}

uses
  DSharp.Core.MethodIntercept,
  Rtti,
  TypInfo;

type
  PVirtualMethodData = ^TVirtualMethodData;
  TVirtualMethodData = packed record
    Equals: Pointer;
    GetHashCode: Pointer;
    ToString: Pointer;
    SafeCallException: Pointer;
    AfterConstruction: Pointer;
    BeforeDestruction: Pointer;
    Dispatch: Pointer;
    DefaultHandler: Pointer;
    NewInstance: Pointer;
    FreeInstance: Pointer;
    Destroy: Pointer;
  end;

  PClassData = ^TClassData;
  TClassData = packed record
    SelfPtr: TClass;
    IntfTable: PInterfaceTable;
    AutoTable: Pointer;
    InitTable: Pointer;
    TypeInfo: PTypeInfo;
    FieldTable: Pointer;
    MethodTable: Pointer;
    DynamicTable: Pointer;
    ClassName: PShortString;
    InstanceSize: Integer;
    Parent: ^TClass;
    VirtualMethods: TVirtualMethodData;
  end;

  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  TVirtualClass = class
  private
    FVirtualMethodTable: PVtable;
    FInstance: TObject;
    FClassData: PClassData;
    FMethodIntercepts: TMethodIntercepts;
    FOnInvoke: TMethodInvokeEvent;
    FFreeOnInstanceDestroy: Boolean;
    FDestroy: Pointer;
    FDestroyStub: array[0..12] of Byte;
    class var FContext: TRttiContext;
    function GetMetaclassType: TClass;
  protected
    procedure DoInvoke(UserData: Pointer;
      const Args: TArray<TValue>; out Result: TValue);
    procedure Virtual_Destroy(Instance: TObject; OuterMost: ShortInt);
  public
    constructor Create(ClassType: TClass); overload;
//    constructor Create(ClassType: TClass; const ImplementedInterfaces: array of TGUID); overload;
    destructor Destroy; override;

    property FreeOnInstanceDestroy: Boolean
      read FFreeOnInstanceDestroy write FFreeOnInstanceDestroy;
    property Instance: TObject read FInstance write FInstance;
    property MetaclassType: TClass read GetMetaclassType;
    property OnInvoke: TMethodInvokeEvent read FOnInvoke write FOnInvoke;
  end;

implementation

uses
  Windows;

{ TVirtualClass }

constructor TVirtualClass.Create(ClassType: TClass);
var
  i: Integer;
  LMaxVirtualIndex: Integer;
  LMethod: TRttiMethod;
  LType: TRttiType;
type
  TProcedure = procedure;
begin
  LType := FContext.GetType(ClassType);
  FMethodIntercepts := TMethodIntercepts.Create;

  LMaxVirtualIndex := 0;
  for LMethod in LType.GetMethods do
  begin
    if LMethod.DispatchKind = dkVtable then
    begin
      if LMethod.VirtualIndex > LMaxVirtualIndex then
      begin
        LMaxVirtualIndex := LMethod.VirtualIndex;
      end;
      if LMethod.VirtualIndex > -1 then
      begin
        FMethodIntercepts.Add(TMethodIntercept.Create(LMethod, DoInvoke));
      end;
    end;
  end;

  GetMem(FClassData, SizeOf(Pointer) * (LMaxVirtualIndex + 1) - vmtSelfPtr);
  Move(PClassData(PByte(ClassType) + vmtSelfPtr)^,
    FClassData^, SizeOf(Pointer) * (LMaxVirtualIndex + 1) - vmtSelfPtr);
  GetMem(FClassData.Parent, SizeOf(Pointer));
  FClassData.Parent^ := ClassType;
  FVirtualMethodTable := PVtable(PByte(FClassData) - vmtSelfPtr);
  FClassData.IntfTable := nil;

  for i := 0 to Pred(FMethodIntercepts.Count) do
  begin
    FVirtualMethodTable[FMethodIntercepts[i].VirtualIndex] := FMethodIntercepts[i].CodeAddress;
  end;

  FDestroy := FClassData.VirtualMethods.Destroy;
  FDestroyStub[0] := $8B;
  FDestroyStub[1] := $CA;
  FDestroyStub[2] := $BA;
  PNativeUInt(@FDestroyStub[3])^ := NativeUInt(Self);
  FDestroyStub[7] := $92;
  FDestroyStub[8] := $E9;
  PNativeUInt(@FDestroyStub[9])^ := NativeUInt(@TVirtualClass.Virtual_Destroy) - NativeUInt(@FDestroyStub[8]) - 5;
  FClassData.VirtualMethods.Destroy := @FDestroyStub;
  VirtualProtect(@FDestroyStub, SizeOf(FDestroyStub), PAGE_EXECUTE_READWRITE, @i);
  FlushInstructionCache(GetCurrentProcess, @FDestroyStub, SizeOf(FDestroyStub));
end;

//constructor TVirtualClass.Create(ClassType: TClass;
//  const ImplementedInterfaces: array of TGUID);
//var
//  i: Integer;
//begin
//  Create(ClassType);
//
//  if Length(ImplementedInterfaces) > 0 then
//  begin
//    Inc(FClassData.InstanceSize, Length(ImplementedInterfaces) * SizeOf(Pointer));
//    GetMem(FClassData.IntfTable, SizeOf(Integer) + Length(ImplementedInterfaces) * SizeOf(TInterfaceEntry));
//    FClassData.IntfTable.EntryCount := Length(ImplementedInterfaces);
//    for i := 0 to High(ImplementedInterfaces) do
//    begin
//      FClassData.IntfTable.Entries[i].IID := ImplementedInterfaces[i];
//      FClassData.IntfTable.Entries[i].IOffset := ClassType.InstanceSize - SizeOf(Pointer) + i * SizeOf(Pointer);
//    end;
//  end;
//end;

destructor TVirtualClass.Destroy;
begin
  FreeMem(FClassData.Parent);
  if Assigned(FClassData.IntfTable) then
    FreeMem(FClassData.IntfTable);
  FreeMem(FClassData);
  FMethodIntercepts.Free;
  inherited;
end;

procedure TVirtualClass.DoInvoke(UserData: Pointer; const Args: TArray<TValue>;
  out Result: TValue);
begin
  if Assigned(FOnInvoke) then
  begin
    FOnInvoke(TMethodIntercept(UserData).Method, Args, Result);
  end;
end;

function TVirtualClass.GetMetaclassType: TClass;
begin
  Result := TClass(PByte(FClassData) - vmtSelfPtr);
end;

procedure TVirtualClass.Virtual_Destroy(Instance: TObject; OuterMost: ShortInt);
type
  TDestroy = procedure(OuterMost: ShortInt) of object;
var
  LDestroy: TDestroy;
begin
  TMethod(LDestroy).Code := FDestroy;
  TMethod(LDestroy).Data := Instance;

  LDestroy(OuterMost);

  if FFreeOnInstanceDestroy then
    Free;
end;

end.
