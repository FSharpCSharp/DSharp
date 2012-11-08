(*
  Copyright (c) 2012, Stefan Glienke
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

unit DSharp.Interception.InterfaceMapping;

interface

uses
  Rtti,
  TypInfo;

type
  TInterfaceMapping = record
  private
    FInterfaceMethods: TArray<TRttiMethod>;
    FInterfaceType: TRttiType;
    FTargetMethods: TArray<TRttiMethod>;
    FTargetType: TRttiType;
  public
    constructor Create(InterfaceType, TargetType: PTypeInfo);

    property InterfaceMethods: TArray<TRttiMethod> read FInterfaceMethods;
    property InterfaceType: TRttiType read FInterfaceType;
    property TargetMethods: TArray<TRttiMethod> read FTargetMethods;
    property TargetType: TRttiType read FTargetType;
  end;

implementation

uses
  DSharp.Core.Reflection;

//type
//  PCallStub = ^TCallStub;
//  TCallStub = record
//  private
//    FCodeAddress: Pointer;
//    FVirtualIndex: SmallInt;
//  public
//    constructor Create(Address: Pointer);
//    property CodeAddress: Pointer read FCodeAddress;
//    property VirtualIndex: SmallInt read FVirtualIndex;
//  end;
//
//constructor TCallStub.Create(Address: Pointer);
//var
//  i: Integer;
//begin
//  FVirtualIndex := -1;
//  i := 0;
//  while True do
//  begin
//    case PByte(Address)[i] of
//      $C3:
//      begin
//        FVirtualIndex := PByte(Address)[i - 4] div SizeOf(Pointer);
//        Break;
//      end;
//      $E9:
//      begin
//        FCodeAddress := Pointer(PNativeUInt(@PByte(Address)[i + 1])^ + NativeUInt(@PByte(Address)[i]) + 5);
//        Break;
//      end;
//    end;
//    Inc(i);
//  end;
//end;

{ TInterfaceMapping }

constructor TInterfaceMapping.Create(InterfaceType, TargetType: PTypeInfo);
//var
//  interfaceEntry: PInterfaceEntry;
//  i, k: Integer;
//  methods: TArray<TRttiMethod>;
//  stub: TCallStub;
type
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
begin
  FInterfaceType := GetRttiType(InterfaceType);
  FInterfaceMethods := FInterfaceType.GetMethods;
  FTargetType := GetRttiType(TargetType);
  SetLength(FTargetMethods, Length(FInterfaceMethods));

//  interfaceEntry := FTargetType.AsInstance.MetaclassType.GetInterfaceEntry(
//    FInterfaceType.AsInterface.GUID);
//  methods := FTargetType.GetMethods;
//  for i := 0 to High(FInterfaceMethods) do
//  begin
//    stub := TCallStub.Create(PVtable(interfaceEntry.VTable)[FInterfaceMethods[i].VirtualIndex]);
//    for k := 0 to High(methods) do
//    begin
//      if stub.VirtualIndex = -1 then
//      begin
//        if methods[k].CodeAddress = stub.CodeAddress then
//        begin
//          FTargetMethods[i] := methods[k];
//          Break;
//        end;
//      end
//      else
//      begin
//        if methods[k].VirtualIndex = stub.VirtualIndex then
//        begin
//          FTargetMethods[i] := methods[k];
//          Break;
//        end;
//      end;
//    end;

//    FTargetMethods[i] := FTargetType.GetMethod(
//      PVtable(interfaceEntry.VTable)[FInterfaceMethods[i].VirtualIndex]);
//  end;
end;

end.
