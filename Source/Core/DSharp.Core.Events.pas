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

unit DSharp.Core.Events;

interface

{$I DSharp.inc}

uses
  Classes,
  DSharp.Core.NotificationHandler,
  Generics.Collections,
  ObjAuto,
{$IFDEF CPUX86}
  ObjAutoPatch,
{$ENDIF}
  Rtti,
  TypInfo;

type
  PMethod = ^TMethod;

  PDelegate = ^IDelegate;
  IDelegate = interface
    procedure Invoke;
  end;

  IEvent = interface
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: TMethod;
    function GetOnChanged: TNotifyEvent;
    procedure Add(const Method: TMethod);
    procedure Assign(Source: IEvent);
    procedure Clear;
    procedure Remove(const Method: TMethod);
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnChanged(const Value: TNotifyEvent);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethod read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  IEvent<T> = interface(IEvent)
    function GetInvoke: T;
    procedure Add(Event: T);
    procedure Assign(Source: IEvent<T>);
    procedure Remove(Event: T);
    property Invoke: T read GetInvoke;
  end;

  TEventBase = class(TInterfacedObject, IEvent)
  strict private
    FEnabled: Boolean;
    FInvoke: TMethod;
    FMethods: TList<TMethod>;
    FOnChanged: TNotifyEvent;
    FTypeInfo: PTypeInfo;

    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetOnChanged: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnChanged(const Value: TNotifyEvent);
  strict protected
    procedure Add(const Method: TMethod);
    procedure Assign(Source: IEvent);
    function Cast(const Value): TMethod; overload;
    procedure Cast(const Method: TMethod; var Value); overload;
    procedure Clear;
    function GetInvoke: TMethod;
    function IndexOf(const Method: TMethod): Integer;
    procedure MethodAdded(const Method: TMethod); virtual;
    procedure MethodRemoved(const Method: TMethod); virtual;
    procedure Notify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification); virtual;
    procedure Remove(const Method: TMethod);
    procedure SetInvoke(const Value: TMethod);

    property Methods: TList<TMethod> read FMethods;
    property TypeInfo: PTypeInfo read FTypeInfo;
  public
    constructor Create(ATypeInfo: PTypeInfo);
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TEvent = class abstract(TEventBase, IDelegate)
  strict private
    FDispatcher: TMethod;
    FNotificationHandler: TNotificationHandler;
    FOwner: TComponent;

    function IndexOfInstance(AInstance: TObject): Integer;
    procedure InternalInvoke(Params: PParameters; StackSize: Integer);
    procedure Invoke;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveInstanceReferences(const AInstance: TObject);
    procedure SetDispatcher(ATypeData: PTypeData);
  strict protected
    procedure MethodAdded(const Method: TMethod); override;
    procedure MethodRemoved(const Method: TMethod); override;
    procedure Notify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification); override;
  public
    constructor Create(ATypeInfo: PTypeInfo; AOwner: TComponent);
    destructor Destroy; override;

    property Owner: TComponent read FOwner;
  end;

  TEvent<T> = class(TEvent, IEvent<T>)
  strict protected
    function GetInvoke: T;
  public
    constructor Create; overload;
    constructor Create(AOwner: TComponent); overload;
    constructor Create(AOwner: TComponent; AEvents: array of T); overload;
{$IF CompilerVersion > 21}
    class function Create<TDelegate>(AOwner: TComponent;
      ADelegates: array of TDelegate): TEvent<T>; overload;
{$IFEND}
    class function Create<TDelegate>(AOwner: TComponent;
      ADelegates: TArray<TDelegate>): TEvent<T>; overload;

    procedure Add(Event: T); overload;
    procedure Add<TDelegate>(Delegate: TDelegate); overload;
    procedure Assign(Source: IEvent<T>);
    procedure Remove(Event: T); overload;
    procedure Remove<TDelegate>(Delegate: TDelegate); overload;
    function IndexOf(Event: T): Integer;
    property Invoke: T read GetInvoke;
  end;

  Event<T> = record
  strict private
    FEventHandler: IEvent<T>;
    FInitialized: Boolean;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetEventHandler: IEvent<T>;
    function GetInvoke: T;
    function GetOnChanged: TNotifyEvent;
    function Initialize: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnChanged(const Value: TNotifyEvent);
  public
    constructor Create(AEventHandler: IEvent<T>);

    procedure Add(const AEvent: T);
    procedure Clear;
    procedure Remove(const AEvent: T);
    procedure Assign(const Source: Event<T>);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property EventHandler: IEvent<T> read GetEventHandler;
    property Invoke: T read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;

    class operator Implicit(const AValue: Event<T>): IEvent<T>;
    class operator Implicit(const AValue: Event<T>): T;
    class operator Implicit(const AValue: IEvent<T>): Event<T>;
  end;

procedure GetMethodTypeData(Method: TRttiMethod; var TypeData: PTypeData);
function IsValid(AObject: TObject): Boolean;
procedure InvokeMethod(const Method: TMethod; Parameters: PParameters; StackSize: Integer);
procedure MethodPointerToMethodReference(const AMethodPointer; const AMethodReference);
procedure MethodReferenceToMethodPointer(const AMethodReference; const AMethodPointer);

implementation

var
  Context: TRttiContext;

procedure GetMethodTypeData(Method: TRttiMethod; var TypeData: PTypeData);

  procedure WriteByte(var Dest: PByte; b: Byte);
  begin
    Dest[0] := b;
    Inc(Dest);
  end;

  procedure WritePackedShortString(var Dest: PByte; const s: string);
  begin
    PShortString(Dest)^ := ShortString(s);
    Inc(Dest, Dest[0] + 1);
  end;

  procedure WritePointer(var Dest: PByte; p: Pointer);
  begin
    PPointer(Dest)^ := p;
    Inc(Dest, SizeOf(Pointer));
  end;

var
  params: TArray<TRttiParameter>;
  i: Integer;
  p: PByte;
begin
  TypeData.MethodKind := Method.MethodKind;
  params := Method.GetParameters;
  TypeData.ParamCount := Length(params);
  p := @TypeData.ParamList;
  for i := Low(params) to High(params) do
  begin
    WriteByte(p, Byte(params[i].Flags));
    WritePackedShortString(p, params[i].Name);
    WritePackedShortString(p, params[i].ParamType.Name);
  end;
  if method.MethodKind = mkFunction then
  begin
    WritePackedShortString(p, method.ReturnType.Name);
    WritePointer(p, method.ReturnType.Handle);
  end;
  WriteByte(p, Byte(method.CallingConvention));
  for i := Low(params) to High(params) do
  begin
    WritePointer(p, Pointer(NativeInt(params[i].ParamType.Handle) - SizeOf(Pointer)));
  end;
end;

function IsValid(AObject: TObject): Boolean;
{$IFDEF VER210}
type
  PNativeInt = ^NativeInt;
{$ENDIF}
begin
  Result := False;
  if Assigned(AObject) then
  try
    if PNativeInt(AObject)^ > $FFFF then  // "hotfix" to prevent some access violations (no clue if this works) :)
      Result := PNativeInt(AObject)^ = PNativeInt(PNativeInt(AObject)^ + vmtSelfPtr)^;
  except
  end;
end;

procedure InvokeMethod(const Method: TMethod;
  Parameters: PParameters; StackSize: Integer);
const
  PointerSize = SizeOf(Pointer);
type
  TParameters = packed record
{$IFDEF CPUX86}
    Registers: array[paEDX..paECX] of Cardinal;
    EAXRegister: Cardinal;
    ReturnAddress: Pointer;
{$ENDIF CPUX86}
    Stack: array[0..1023] of Byte;
  end;
{$IF Defined(CPUX86)}
asm
  push ebp
  mov ebp,esp
  push eax // ebp-4 = Method
  push ebx
  mov ebx, edx // ebx = Parameters

  // if StackSize > 0
  test ecx,ecx
  jz @@no_stack

  // stack address alignment
  add ecx,PointerSize-1
  and ecx,not(PointerSize-1)
  and ecx,$ffff
  sub esp,ecx

  // put stack address as second parameter
  mov edx,esp

  // put params on stack as first parameter
  lea eax,[ebx].TParameters.Stack

  call Move

@@no_stack:
  mov edx,[ebx].TParameters.Registers.dword[0]
  mov ecx,[ebx].TParameters.Registers.dword[4]
  mov ebx,[ebp-$04]
  mov eax,[ebx].TMethod.Data
  call [ebx].TMethod.Code

  pop ebx
  pop eax
  mov esp,ebp
  pop ebp
end;
{$ELSEIF Defined(CPUX64)}
asm
  .params 60
  mov [rbp+$200],Method
  mov [rbp+$208],Parameters
  test r8,r8
  jz @@no_stack

  // put params on stack as first parameter
  lea rcx,[Parameters].TParameters.Stack

  // put stack address as second parameter
  mov rdx,rsp

  call Move

  mov rdx,[rbp+$208]

@@no_stack:
  mov rcx,[rdx].TParameters.Stack.qword[0]
  mov r8,[rdx].TParameters.Stack.qword[16]
  mov r9,[rdx].TParameters.Stack.qword[24]

  movsd xmm0,[rdx].TParameters.Stack.qword[0]
  movsd xmm1,[rdx].TParameters.Stack.qword[8]
  movsd xmm2,[rdx].TParameters.Stack.qword[16]
  movsd xmm3,[rdx].TParameters.Stack.qword[24]

  mov rdx,[rdx].TParameters.Stack.qword[8]

  mov rax,[rbp+$200]
  lea rax,[rax]
  mov rcx,[rax].TMethod.Data
  call [rax].TMethod.Code
end;
{$IFEND}

procedure MethodReferenceToMethodPointer(const AMethodReference; const AMethodPointer);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  PMethod(@AMethodPointer).Code := PPVtable(AMethodReference)^^[3];
  PMethod(@AMethodPointer).Data := Pointer(AMethodReference);
end;

procedure MethodPointerToMethodReference(const AMethodPointer; const AMethodReference);
begin
  PPointer(@AMethodReference)^ := PMethod(@AMethodPointer).Data;
  IInterface(AMethodReference)._AddRef;
end;

{ TEventBase }

constructor TEventBase.Create(ATypeInfo: PTypeInfo);
begin
  FEnabled := True;
  FMethods := TList<TMethod>.Create();
  FMethods.OnNotify := Notify;
  FTypeInfo := ATypeInfo;
end;

destructor TEventBase.Destroy;
begin
  FMethods.Free;
end;

procedure TEventBase.Add(const Method: TMethod);
begin
  FMethods.Add(Method);
  MethodAdded(Method);
end;

procedure TEventBase.Assign(Source: IEvent);
begin
  FMethods.Clear;
  FMethods.AddRange((Source as TEventBase).Methods);
end;

function TEventBase.Cast(const Value): TMethod;
begin
  if FTypeInfo.Kind = tkInterface then
  begin
    MethodReferenceToMethodPointer(Value, Result);
  end
  else
  begin
    Result := PMethod(@Value)^;
  end;
end;

procedure TEventBase.Cast(const Method: TMethod; var Value);
begin
  if FTypeInfo.Kind = tkInterface then
  begin
    MethodPointerToMethodReference(Method, Value);
  end
  else
  begin
    PMethod(@Value)^ := Method;
  end;
end;

procedure TEventBase.Clear;
begin
  FMethods.Clear;
end;

function TEventBase.GetCount: Integer;
begin
  Result := FMethods.Count;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TEventBase.GetInvoke: TMethod;
begin
  Result := FInvoke;
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := FOnChanged;
end;

function TEventBase.IndexOf(const Method: TMethod): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(FMethods.Count) do
  begin
    if (FMethods[i].Code = Method.Code)
      and (FMethods[i].Data = Method.Data) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TEventBase.MethodAdded(const Method: TMethod);
begin

end;

procedure TEventBase.MethodRemoved(const Method: TMethod);
begin

end;

procedure TEventBase.Notify(Sender: TObject; const Item: TMethod;
  Action: TCollectionNotification);
begin
  if Assigned(FOnChanged) then
  begin
    FOnChanged(Self);
  end;
end;

procedure TEventBase.Remove(const Method: TMethod);
var
  i: Integer;
begin
  i := IndexOf(Method);
  if i > -1 then
  begin
    FMethods.Delete(i);
  end;
  MethodRemoved(Method);
end;

procedure TEventBase.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TEventBase.SetInvoke(const Value: TMethod);
begin
  FInvoke := Value;
end;

procedure TEventBase.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

{ TEvent }

constructor TEvent.Create(ATypeInfo: PTypeInfo; AOwner: TComponent);
var
  LMethods: TArray<TRttiMethod>;
  LRttiType: TRttiType;
  LTypeData: PTypeData;
begin
  inherited Create(ATypeInfo);

  LTypeData := GetTypeData(TypeInfo);
  if TypeInfo.Kind = tkInterface then
  begin
    LRttiType := Context.GetType(TypeInfo);
    LMethods := LRttiType.GetMethods;
    Assert(Length(LMethods) > 0, UTF8ToString(TypeInfo.Name) + ' must contain extended RTTI');
    New(LTypeData);
    try
      GetMethodTypeData(LMethods[0], LTypeData);
      Assert(LTypeData.MethodKind = mkProcedure, UTF8ToString(TypeInfo.Name) + ' must not be a function');
      SetDispatcher(LTypeData);
    finally
      Dispose(LTypeData);
    end;
  end
  else
  begin
    Assert(TypeInfo.Kind = tkMethod, UTF8ToString(TypeInfo.Name) + ' must be a method pointer type');
    Assert(LTypeData.MethodKind = mkProcedure, UTF8ToString(TypeInfo.Name) + ' must not be a function');
    SetDispatcher(LTypeData);
  end;

  FNotificationHandler := TNotificationHandler.Create(Self, Notification);
  if Assigned(AOwner) then
  begin
    FOwner := AOwner;
    FOwner.FreeNotification(FNotificationHandler);
  end;
end;

destructor TEvent.Destroy;
begin
  FNotificationHandler.Free();
  ReleaseMethodPointer(FDispatcher);
  inherited;
end;

procedure TEvent.InternalInvoke(Params: PParameters; StackSize: Integer);
var
  i: Integer;
begin
  if Enabled then
    for i := 0 to Methods.Count - 1 do
      InvokeMethod(Methods[i], Params, StackSize);
end;

procedure TEvent.Invoke;
asm
{$IFDEF CPUX64}
  push [rcx].FDispatcher.Code
  mov rcx,[rcx].FDispatcher.Data
{$ELSE}
  push [eax].FDispatcher.Code
  mov eax,[eax].FDispatcher.Data
{$ENDIF}
end;

function TEvent.IndexOfInstance(AInstance: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(Methods.Count) do
  begin
    if TObject(Methods[i].Data) = AInstance then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TEvent.MethodAdded(const Method: TMethod);
begin
  inherited;
  if IsValid(Method.Data) and (TObject(Method.Data) is TComponent) then
  begin
    FNotificationHandler.FreeNotification(TComponent(Method.Data));
  end;
end;

procedure TEvent.MethodRemoved(const Method: TMethod);
begin
  inherited;
  if IsValid(Method.Data) and (TObject(Method.Data) is TComponent)
    and (IndexOfInstance(TObject(Method.Data)) < 0) then
  begin
    FNotificationHandler.RemoveFreeNotification(TComponent(Method.Data));
  end;
end;

procedure TEvent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    RemoveInstanceReferences(AComponent);
    if (AComponent = FOwner) and (RefCount = 0) then
    begin
      Free();
    end;
  end;
end;

procedure TEvent.Notify(Sender: TObject; const Item: TMethod;
  Action: TCollectionNotification);
begin
  if (TypeInfo.Kind = tkInterface)
    and Assigned(Item.Data) and not IsValid(Item.Data) then
  begin
    case Action of
      cnAdded: IInterface(Item.Data)._AddRef();
      cnRemoved: IInterface(Item.Data)._Release();
    end;
  end;

  inherited Notify(Sender, Item, Action);
end;

procedure TEvent.RemoveInstanceReferences(const AInstance: TObject);
var
  i: Integer;
begin
  repeat
    i := IndexOfInstance(AInstance);
    if i > -1 then
    begin
      Methods.Delete(i);
    end;
  until i = -1;
end;

procedure TEvent.SetDispatcher(ATypeData: PTypeData);
var
  LInvoke: IDelegate;
begin
  if Assigned(FDispatcher.Code)
    and Assigned(FDispatcher.Data) then
  begin
    ReleaseMethodPointer(FDispatcher);
  end;
  FDispatcher := CreateMethodPointer(InternalInvoke, ATypeData);

  if TypeInfo.Kind = tkInterface then
  begin
    LInvoke := Self;
    SetInvoke(Cast(LInvoke));
  end
  else
  begin
    SetInvoke(FDispatcher);
  end;
end;

{ TEvent<T> }

constructor TEvent<T>.Create;
begin
  Create(nil);
end;

constructor TEvent<T>.Create(AOwner: TComponent);
begin
  inherited Create(System.TypeInfo(T), AOwner);
end;

constructor TEvent<T>.Create(AOwner: TComponent; AEvents: array of T);
var
  i: Integer;
begin
  Create(AOwner);
  for i := Low(AEvents) to High(AEvents) do
  begin
    Add(AEvents[i]);
  end;
end;

{$IF CompilerVersion > 21}
class function TEvent<T>.Create<TDelegate>(AOwner: TComponent;
  ADelegates: array of TDelegate): TEvent<T>;
var
  LDelegate: TDelegate;
begin
  Result := Create(AOwner);
  for LDelegate in ADelegates do
  begin
    Result.Add<TDelegate>(LDelegate);
  end;
end;
{$IFEND}

class function TEvent<T>.Create<TDelegate>(AOwner: TComponent;
  ADelegates: TArray<TDelegate>): TEvent<T>;
var
  LDelegate: TDelegate;
begin
  Result := Create(AOwner);
  for LDelegate in ADelegates do
  begin
    Result.Add<TDelegate>(LDelegate);
  end;
end;

procedure TEvent<T>.Add(Event: T);
begin
  inherited Add(Cast(Event));
end;

procedure TEvent<T>.Add<TDelegate>(Delegate: TDelegate);
var
  LEvent: T;
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := System.TypeInfo(TDelegate);
  Assert(LTypeInfo.Kind = tkInterface, 'TDelegate must be a method reference');

  MethodReferenceToMethodPointer(Delegate, LEvent);
  Add(LEvent);
end;

procedure TEvent<T>.Assign(Source: IEvent<T>);
begin
  inherited Assign(Source);
end;

function TEvent<T>.GetInvoke: T;
begin
  Cast(inherited GetInvoke(), Result);
end;

function TEvent<T>.IndexOf(Event: T): Integer;
begin
  Result := inherited IndexOf(Cast(Event));
end;

procedure TEvent<T>.Remove(Event: T);
begin
  inherited Remove(Cast(Event));
end;

procedure TEvent<T>.Remove<TDelegate>(Delegate: TDelegate);
var
  LEvent: T;
begin
  MethodReferenceToMethodPointer(Delegate, LEvent);
  Remove(LEvent);
end;

{ Event<T> }

constructor Event<T>.Create(AEventHandler: IEvent<T>);
begin
  FEventHandler := AEventHandler;
  FInitialized := Assigned(FEventHandler);
end;

procedure Event<T>.Add(const AEvent: T);
begin
  if Initialize then
  begin
    FEventHandler.Add(AEvent);
  end;
end;

procedure Event<T>.Assign(const Source: Event<T>);
begin
  if Initialize then
  begin
    FEventHandler.Assign(Source.EventHandler);
  end;
end;

procedure Event<T>.Clear;
begin
  if Initialize then
  begin
    FEventHandler.Clear;
  end;
end;

function Event<T>.GetCount: Integer;
begin
  Result := 0;
  if Initialize then
  begin
    Result := FEventHandler.Count;
  end;
end;

function Event<T>.GetEnabled: Boolean;
begin
  Result := False;
  if Initialize then
  begin
    Result := FEventHandler.Enabled;
  end;
end;

function Event<T>.GetEventHandler: IEvent<T>;
begin
  Initialize;
  Result := FEventHandler;
end;

function Event<T>.GetInvoke: T;
begin
  if Initialize then
  begin
    Result := FEventHandler.Invoke;
  end;
end;

function Event<T>.GetOnChanged: TNotifyEvent;
begin
  if Initialize then
  begin
    Result := FEventHandler.OnChanged;
  end;
end;

function Event<T>.Initialize: Boolean;
begin
  if not FInitialized then
  begin
    FEventHandler := TEvent<T>.Create;
    FInitialized := True;
  end;
  Result := Assigned(FEventHandler);
end;

procedure Event<T>.Remove(const AEvent: T);
begin
  if Initialize then
  begin
    FEventHandler.Remove(AEvent);
  end;
end;

procedure Event<T>.SetEnabled(const Value: Boolean);
begin
  if Initialize then
  begin
    FEventHandler.Enabled := Value;
  end;
end;

procedure Event<T>.SetOnChanged(const Value: TNotifyEvent);
begin
  if Initialize then
  begin
    FEventHandler.OnChanged := Value;
  end;
end;

class operator Event<T>.Implicit(const AValue: Event<T>): IEvent<T>;
begin
  Result := AValue.EventHandler;
end;

class operator Event<T>.Implicit(const AValue: Event<T>): T;
begin
  Result := AValue.EventHandler.Invoke;
end;

class operator Event<T>.Implicit(const AValue: IEvent<T>): Event<T>;
begin
  Result := Event<T>.Create(AValue);
end;

end.
