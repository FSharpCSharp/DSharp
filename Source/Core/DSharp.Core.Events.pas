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

uses
  Classes,
  DSharp.Core.NotificationHandler,
  Generics.Collections,
  ObjAuto,
{$IF CompilerVersion < 23}
  ObjAutoPatch,
{$IFEND}
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
    procedure Add(const AEvent: TMethod);
    procedure Assign(Source: IEvent);
    procedure Clear;
    procedure Remove(const AEvent: TMethod);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetOnChanged(const Value: TNotifyEvent);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethod read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  TEvent = class abstract(TInterfacedObject, IEvent, IDelegate)
  strict private
    FDispatcher: TMethod;
    FEnabled: Boolean;
    FInvoke: TMethod;
    FMethods: TList<TMethod>;
    FNotificationHandler: TNotificationHandler;
    FOnChanged: TNotifyEvent;
    FOwner: TComponent;
    FTypeInfo: PTypeInfo;
{$IF CompilerVersion > 22}
    FCallingConvention: TCallConv;
    FParameters: TArray<TRttiParameter>;
{$IFEND}
    function GetOnChanged: TNotifyEvent;
    function IndexOfInstance(AInstance: TObject): Integer;
    procedure InternalInvoke(Params: PParameters; StackSize: Integer);
    procedure Invoke;
    procedure MethodAdded(const AMethod: TMethod);
    procedure MethodRemoved(const AMethod: TMethod);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveInstanceReferences(const AInstance: TObject);
    procedure SetDispatcher(ATypeData: PTypeData);
    procedure SetOnChanged(const Value: TNotifyEvent);
  strict protected
    procedure Add(const AEvent: TMethod);
    procedure Assign(Source: IEvent);
    function Cast(const Value): TMethod; overload;
    procedure Cast(const Method: TMethod; var Value); overload;
    procedure Clear;
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: TMethod;
    function IndexOf(const AEvent: TMethod): Integer;
    procedure Notify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification); virtual;
    procedure Remove(const AEvent: TMethod);
    procedure SetEnabled(const AValue: Boolean);
    property Methods: TList<TMethod> read FMethods;
  public
    constructor Create(ATypeInfo: PTypeInfo; AOwner: TComponent);
    destructor Destroy; override;

    property Owner: TComponent read FOwner;
  end;

  IEvent<T> = interface(IEvent)
    function GetInvoke: T;
    procedure Add(AEvent: T);
    procedure Assign(Source: IEvent<T>);
    procedure Remove(AEvent: T);
    property Invoke: T read GetInvoke;
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

    procedure Add(AEvent: T); overload;
    procedure Add<TDelegate>(ADelegate: TDelegate); overload;
    procedure Assign(Source: IEvent<T>);
    procedure Remove(AEvent: T); overload;
    procedure Remove<TDelegate>(ADelegate: TDelegate); overload;
    function IndexOf(AEvent: T): Integer;
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
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

procedure PassArg(Par: TRttiParameter; Params: PParameters; var Dest: TValue;
  CC: TCallConv; const Index: Integer; var Offset: Byte);
const
  PointerSize = SizeOf(Pointer);
begin
{$IFDEF CPUX64}
  if Par.Flags * [pfVar, pfConst, pfOut] <> [] then
  begin
    Dest := TValue.From<Pointer>(PPointer(@Params.Stack[Offset])^);
  end
  else
  begin
    TValue.Make(Pointer(@Params.Stack[Offset]), Par.ParamType.Handle, Dest);
  end;
  Inc(Offset, PointerSize);
{$ENDIF}

{$IFDEF CPUX86}
  if (CC = ccReg) and (Index < 2) then
  begin
    if Par.Flags * [pfVar, pfConst, pfOut] <> [] then
    begin
      Dest := TValue.From<Pointer>(Pointer(Params.Registers[Index + 1]));
    end
    else
    begin
      TValue.Make(NativeInt(Params.Registers[Index + 1]), Par.ParamType.Handle, Dest);
    end
  end
  else
  begin
    Dec(Offset, PointerSize);
    if Par.Flags * [pfVar, pfConst, pfOut] <> [] then
    begin
      Dest := TValue.From<Pointer>(PPointer(@Params.Stack[Offset])^);
    end
    else
    begin
      TValue.Make(Pointer(@Params.Stack[Offset]), Par.ParamType.Handle, Dest);
    end;
  end;
{$ENDIF}
end;

{ TEvent }

constructor TEvent.Create(ATypeInfo: PTypeInfo; AOwner: TComponent);
var
  LMethods: TArray<TRttiMethod>;
  LRttiType: TRttiType;
  LTypeData: PTypeData;
begin
  FEnabled := True;
  FMethods := TList<TMethod>.Create();
  FMethods.OnNotify := Notify;
  FTypeInfo := ATypeInfo;

  LTypeData := GetTypeData(FTypeInfo);
  if FTypeInfo.Kind = tkInterface then
  begin
    LRttiType := Context.GetType(FTypeInfo);
    LMethods := LRttiType.GetMethods;
    Assert(Length(LMethods) > 0, string(FTypeInfo.Name) + ' must contain extended RTTI');
    New(LTypeData);
    try
      GetMethodTypeData(LMethods[0], LTypeData);
      Assert(LTypeData.MethodKind = mkProcedure, string(FTypeInfo.Name) + ' must not be a function');
{$IF CompilerVersion > 22}
      FCallingConvention := LMethods[0].CallingConvention;
      FParameters := LMethods[0].GetParameters();
{$IFEND}
      SetDispatcher(LTypeData);
    finally
      Dispose(LTypeData);
    end;
  end
  else
  begin
    Assert(FTypeInfo.Kind = tkMethod, string(FTypeInfo.Name) + ' must be a method pointer type');
    Assert(LTypeData.MethodKind = mkProcedure, string(FTypeInfo.Name) + ' must not be a function');
{$IF CompilerVersion > 22}
    LRttiType := Context.GetType(FTypeInfo);
    FCallingConvention := TRttiInvokableType(LRttiType).CallingConvention;
    FParameters := TRttiInvokableType(LRttiType).GetParameters();
{$IFEND}
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
  FMethods.Free();
  ReleaseMethodPointer(FDispatcher);
  inherited;
end;

procedure TEvent.Add(const AEvent: TMethod);
begin
  FMethods.Add(AEvent);
  MethodAdded(AEvent);
end;

procedure TEvent.Assign(Source: IEvent);
begin
  FMethods.Clear;
  FMethods.AddRange((Source as TEvent).FMethods);
end;

function TEvent.Cast(const Value): TMethod;
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

procedure TEvent.Cast(const Method: TMethod; var Value);
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

procedure TEvent.Clear;
begin
  FMethods.Clear;
end;

function TEvent.GetCount: Integer;
begin
  Result := FMethods.Count;
end;

function TEvent.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TEvent.GetInvoke: TMethod;
begin
  Result := FInvoke;
end;

function TEvent.GetOnChanged: TNotifyEvent;
begin
  Result := FOnChanged;
end;

procedure TEvent.InternalInvoke(Params: PParameters; StackSize: Integer);
const
  PointerSize = SizeOf(Pointer);
var
  LMethod: TMethod;
{$IF CompilerVersion < 23}
begin
  if FEnabled then
  begin
    for LMethod in FMethods do
    begin
      // "Push" parameters on stack
      if StackSize > 0 then
      asm
        // Put StackSize as third parameter
        MOV ECX,StackSize
        // stack address alignment
        ADD ECX,PointerSize-1
        AND ECX,NOT(PointerSize-1)
        AND ECX,$FFFF
        SUB ESP,ECX
        // Put Stack Address as second parameter
        MOV EDX,ESP
        // Put Params on Stack as first parameter
        MOV EAX,Params
        LEA EAX,[EAX].TParameters.Stack[8]
        CALL System.Move
      end;
      asm
        MOV EAX,Params
        MOV EDX,[EAX].TParameters.Registers.DWORD[0]
        MOV ECX,[EAX].TParameters.Registers.DWORD[4]
        MOV EAX,LMethod.Data
        CALL LMethod.Code
      end;
    end;
  end;
end;
{$ELSE}
  i: Integer;
  LArgs: TArray<TValue>;
  LOffset: Byte;
begin
  if FEnabled and (FMethods.Count > 0) then
  begin
{$IFDEF CPUX86}
    LOffset := StackSize;
{$ENDIF}
{$IFDEF CPUX64}
    LOffset := PointerSize;
{$ENDIF}
    SetLength(LArgs, Length(FParameters) + 1);

    for i := Low(FParameters) to High(FParameters) do
    begin
      PassArg(FParameters[i], Params, LArgs[i + 1],
        FCallingConvention, i, LOffset);
    end;

    for LMethod in FMethods do
    begin
      LArgs[0] := TValue.From<TObject>(LMethod.Data);
      // workaround for incorrect type guess in Rtti.pas
      TValueData(LArgs[0]).FTypeInfo := TypeInfo(TObject);
      Rtti.Invoke(LMethod.Code, LArgs, FCallingConvention, nil);
    end;
  end;
end;
{$IFEND}

procedure TEvent.Invoke;
asm
{$IFDEF CPUX64}
  PUSH [RCX].FDispatcher.Code
  MOV RCX,[RCX].FDispatcher.Data
{$ELSE}
  PUSH [EAX].FDispatcher.Code
  MOV EAX,[EAX].FDispatcher.Data
{$ENDIF}
end;

function TEvent.IndexOf(const AEvent: TMethod): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(FMethods.Count) do
  begin
    if (FMethods[i].Code = TMethod(AEvent).Code)
      and (FMethods[i].Data = TMethod(AEvent).Data) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TEvent.IndexOfInstance(AInstance: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(FMethods.Count) do
  begin
    if TObject(FMethods[i].Data) = AInstance then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TEvent.MethodAdded(const AMethod: TMethod);
begin
  inherited;
  if IsValid(AMethod.Data) and (TObject(AMethod.Data) is TComponent) then
  begin
    FNotificationHandler.FreeNotification(TComponent(AMethod.Data));
  end;
end;

procedure TEvent.MethodRemoved(const AMethod: TMethod);
begin
  inherited;
  if IsValid(AMethod.Data) and (TObject(AMethod.Data) is TComponent)
    and (IndexOfInstance(TObject(AMethod.Data)) < 0) then
  begin
    FNotificationHandler.RemoveFreeNotification(TComponent(AMethod.Data));
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
  if Assigned(Item.Data) and not IsValid(Item.Data) then
  begin
    case Action of
      cnAdded: IInterface(Item.Data)._AddRef();
      cnRemoved: IInterface(Item.Data)._Release();
    end;
  end;

  if Assigned(FOnChanged) then
  begin
    FOnChanged(Self);
  end;
end;

procedure TEvent.Remove(const AEvent: TMethod);
var
  i: Integer;
begin
  i := IndexOf(AEvent);
  if i > -1 then
  begin
    FMethods.Delete(i);
  end;
  MethodRemoved(AEvent);
end;

procedure TEvent.RemoveInstanceReferences(const AInstance: TObject);
var
  i: Integer;
begin
  repeat
    i := IndexOfInstance(AInstance);
    if i > -1 then
    begin
      FMethods.Delete(i);
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

  if FTypeInfo.Kind = tkInterface then
  begin
    LInvoke := Self;
    FInvoke := Cast(LInvoke);
  end
  else
  begin
    FInvoke := FDispatcher;
  end;
end;

procedure TEvent.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TEvent.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

{ TEvent<T> }

constructor TEvent<T>.Create;
begin
  Create(nil);
end;

constructor TEvent<T>.Create(AOwner: TComponent);
begin
  inherited Create(TypeInfo(T), AOwner);
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

procedure TEvent<T>.Add(AEvent: T);
begin
  inherited Add(Cast(AEvent));
end;

procedure TEvent<T>.Add<TDelegate>(ADelegate: TDelegate);
var
  LEvent: T;
  LTypeInfo: PTypeInfo;
  LTypeData: PTypeData;
{$IF CompilerVersion > 22}
//  LMethod: TRttiMethod;
//  LParams: TArray<TRttiParameter>;
{$IFEND}
begin
  LTypeInfo := TypeInfo(TDelegate);
  Assert(LTypeInfo.Kind = tkInterface, 'TDelegate must be a method reference');
  LTypeInfo := TypeInfo(T);
  LTypeData := GetTypeData(LTypeInfo);

{$IF CompilerVersion > 22}
//  Does not work right now because method references are missing RTTI
//  LMethod := Context.GetType(TypeInfo(TDelegate)).GetMethod('Invoke');
//  Assert(LMethod.MethodKind = LTypeData.MethodKind, 'MethodKind does not match');
//  LParams := LMethod.GetParameters();
//  Assert(Length(LParams) = LTypeData.ParamCount, 'ParamCount does not match');
{$IFEND}
  MethodReferenceToMethodPointer(ADelegate, LEvent);
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

function TEvent<T>.IndexOf(AEvent: T): Integer;
begin
  Result := inherited IndexOf(Cast(AEvent));
end;

procedure TEvent<T>.Remove(AEvent: T);
begin
  inherited Remove(Cast(AEvent));
end;

procedure TEvent<T>.Remove<TDelegate>(ADelegate: TDelegate);
var
  LEvent: T;
begin
  MethodReferenceToMethodPointer(ADelegate, LEvent);
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
