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

unit DSharp.Core.VirtualInterface;

interface

uses
  Generics.Collections,
  Rtti,
  TypInfo;

type
  TVirtualInterfaceInvokeEvent = reference to procedure(Method: TRttiMethod;
    const Args: TArray<TValue>; out Result: TValue);

  TVirtualInterface = class(TInterfacedObject, IInterface)
  private
    type
      TMethodIntercept = class
      private
        FImplementation: TMethodImplementation;
        FMethod: TRttiMethod;
        function GetCodeAddress: Pointer;
        function GetVirtualIndex: SmallInt;
      public
        constructor Create(const Method: TRttiMethod;
          const Callback: TMethodImplementationCallback);
        destructor Destroy; override;
        property CodeAddress: Pointer read GetCodeAddress;
        property VirtualIndex: SmallInt read GetVirtualIndex;
      end;
    class var
      FContext: TRttiContext;
  private
    FVirtualMethodTable: Pointer;
    FInterfaceID: TGUID;
    FMethodIntercepts: TObjectList<TMethodIntercept>;
    FOnInvoke: TVirtualInterfaceInvokeEvent;

    function Virtual_AddRef: Integer; stdcall;
    function Virtual_Release: Integer; stdcall;
    function VirtualQueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    procedure DoInvoke(UserData: Pointer;
      const Args: TArray<TValue>; out Result: TValue);
    procedure ErrorProc;
  public
    constructor Create(TypeInfo: PTypeInfo); overload;
    constructor Create(TypeInfo: PTypeInfo;
      InvokeEvent: TVirtualInterfaceInvokeEvent); overload;
    destructor Destroy; override;
    property OnInvoke: TVirtualInterfaceInvokeEvent read FOnInvoke write FOnInvoke;
  end;

implementation

uses
  RTLConsts;

{ TVirtualInterface }

constructor TVirtualInterface.Create(TypeInfo: PTypeInfo);
var
  i: Integer;
  LMaxVirtualIndex: SmallInt;
  LMethod: TRttiMethod;
  LType: TRttiType;
type
  PVtable = ^TVtable;
  TVtable = array[0..2] of Pointer;
begin
  LType := FContext.GetType(TypeInfo);
  FInterfaceID := TRttiInterfaceType(LType).GUID;
  FMethodIntercepts := TObjectList<TMethodIntercept>.Create();

  LMaxVirtualIndex := 2;
  for LMethod in LType.GetMethods() do
  begin
    if LMaxVirtualIndex < LMethod.VirtualIndex then
    begin
      LMaxVirtualIndex := LMethod.VirtualIndex;
    end;
    FMethodIntercepts.Add(TMethodIntercept.Create(LMethod, DoInvoke));
  end;

  FVirtualMethodTable := AllocMem(SizeOf(Pointer)* (LMaxVirtualIndex + 1));

  PVtable(FVirtualMethodTable)^[0] := @TVirtualInterface.VirtualQueryInterface;
  PVtable(FVirtualMethodTable)^[1] := @TVirtualInterface.Virtual_AddRef;
  PVtable(FVirtualMethodTable)^[2] := @TVirtualInterface.Virtual_Release;

  for i := 0 to Pred(FMethodIntercepts.Count) do
  begin
    PVtable(FVirtualMethodTable)^[FMethodIntercepts[i].VirtualIndex] := FMethodIntercepts[i].CodeAddress;
  end;

  for i := 3 to LMaxVirtualIndex do
  begin
    if not Assigned(PVtable(FVirtualMethodTable)^[i]) then
    begin
      PVtable(FVirtualMethodTable)^[i] := @TVirtualInterface.ErrorProc;
    end;
  end;
end;

constructor TVirtualInterface.Create(TypeInfo: PTypeInfo;
  InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(TypeInfo);
  FOnInvoke := InvokeEvent;
end;

destructor TVirtualInterface.Destroy;
begin
  if Assigned(FVirtualMethodTable) then
  begin
    FreeMem(FVirtualMethodTable);
  end;
  FMethodIntercepts.Free;
  inherited;
end;

procedure TVirtualInterface.DoInvoke(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
begin
  if Assigned(FOnInvoke) then
  begin
    FOnInvoke(TMethodIntercept(UserData).FMethod, Args, Result);
  end;
end;

procedure TVirtualInterface.ErrorProc;
begin
  raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

function TVirtualInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = FInterfaceID then
  begin
    _AddRef();
    Pointer(Obj) := @FVirtualMethodTable;
    Result := S_OK;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TVirtualInterface.VirtualQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.FVirtualMethodTable) - PByte(Self))).QueryInterface(IID, Obj);
end;

function TVirtualInterface.Virtual_AddRef: Integer;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.FVirtualMethodTable) - PByte(Self)))._AddRef();
end;

function TVirtualInterface.Virtual_Release: Integer;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.FVirtualMethodTable) - PByte(Self)))._Release();
end;

function TVirtualInterface._AddRef: Integer;
begin
  Result := inherited;
end;

function TVirtualInterface._Release: Integer;
begin
  Result := inherited;
end;

{ TVirtualInterface.TMethodIntercept }

constructor TVirtualInterface.TMethodIntercept.Create(
  const Method: TRttiMethod; const Callback: TMethodImplementationCallback);
begin
  FImplementation := Method.CreateImplementation(Self, Callback);
  FMethod := Method;
end;

destructor TVirtualInterface.TMethodIntercept.Destroy;
begin
  FImplementation.Free();
  inherited;
end;

function TVirtualInterface.TMethodIntercept.GetCodeAddress: Pointer;
begin
  Result := FImplementation.CodeAddress;
end;

function TVirtualInterface.TMethodIntercept.GetVirtualIndex: SmallInt;
begin
  Result := FMethod.VirtualIndex;
end;

end.
