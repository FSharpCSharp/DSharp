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
  IEvent = interface
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: TMethod;
    procedure Add(const AEvent: TMethod);
    procedure Remove(const AEvent: TMethod);
    procedure SetEnabled(const AValue: Boolean);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethod read GetInvoke;
  end;

  TEvent = class abstract(TInterfacedObject, IEvent)
  strict private
    FEnabled: Boolean;
    FInternalDispatcher: TMethod;
    FMethods: TList<TMethod>;
    function IEvent.GetInvoke = GetInvokeBase;
    procedure InternalInvoke(Params: PParameters; StackSize: Integer);
    procedure InternalNotify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification);
  strict protected
{$IF CompilerVersion > 22}
    FInvokableType: TRttiInvokableType;
    FParameters: TArray<TRttiParameter>;
{$IFEND}
    function GetInvokeBase: TMethod; virtual; abstract;
    procedure MethodAdded(const AMethod: TMethod); virtual; abstract;
    procedure MethodRemoved(const AMethod: TMethod); virtual; abstract;
    procedure Add(const AEvent: TMethod);
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function IndexOf(const AEvent: TMethod): Integer;
    function IndexOfInstance(const AInstance: TObject): Integer;
    procedure Remove(const AEvent: TMethod);
    procedure RemoveInstanceReferences(const AInstance: TObject);
    procedure SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData);
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  IEvent<T> = interface(IEvent)
    function GetCount: Integer;
    function GetInvoke: T;
    function GetOnChanged: TNotifyEvent;
    procedure Add(AEvent: T);
    procedure Remove(AEvent: T);
    procedure SetOnChanged(const Value: TNotifyEvent);
    property Count: Integer read GetCount;
    property Invoke: T read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  TEvent<T> = class(TEvent, IEvent<T>)
  strict private
    FInvoke: T;
    FNotificationHandler: TNotificationHandler<TEvent<T>>;
    FOwner: TComponent;
    FOnChanged: TNotifyEvent;
    function GetInvoke: T;
    function GetOnChanged: TNotifyEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure SetEventDispatcher(var ADispatcher: T; ATypeData: PTypeData);
    procedure SetOnChanged(const Value: TNotifyEvent);
  strict protected
    function GetInvokeBase: TMethod; override;
    procedure MethodAdded(const AMethod: TMethod); override;
    procedure MethodRemoved(const AMethod: TMethod); override;
  public
    constructor Create(AOwner: TComponent); overload;
    constructor Create(AOwner: TComponent; AEvents: array of T); overload;
{$IF CompilerVersion > 21}
    class function Create<TDelegate>(AOwner: TComponent;
      ADelegates: array of TDelegate): TEvent<T>; overload;
{$IFEND}
    class function Create<TDelegate>(AOwner: TComponent;
      ADelegates: TArray<TDelegate>): TEvent<T>; overload;
    destructor Destroy; override;
    procedure Add(AEvent: T); overload;
    procedure Add<TDelegate>(ADelegate: TDelegate); overload;
    procedure Remove(AEvent: T); overload;
    procedure Remove<TDelegate>(ADelegate: TDelegate); overload;
    function IndexOf(AEvent: T): Integer;
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: T read GetInvoke;
    property Owner: TComponent read FOwner;
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
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnChanged(const Value: TNotifyEvent);
  public
    constructor Create(AEventHandler: IEvent<T>);

    procedure Add(AEvent: T);
    procedure Remove(AEvent: T);
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property EventHandler: IEvent<T> read GetEventHandler;
    property Invoke: T read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;

    class operator Implicit(const AValue: Event<T>): IEvent<T>;
    class operator Implicit(const AValue: Event<T>): T;
    class operator Implicit(const AValue: IEvent<T>): Event<T>;
  end;

function IsValid(AObject: TObject): Boolean;
procedure MethodReferenceToMethodPointer(const AMethodReference; var AMethodPointer);

var
  Context: TRttiContext;

implementation

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

procedure MethodReferenceToMethodPointer(const AMethodReference; var AMethodPointer);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  TMethod(AMethodPointer).Code := PPVtable(AMethodReference)^^[3];
  TMethod(AMethodPointer).Data := Pointer(AMethodReference);
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

{ TEventHandler }

constructor TEvent.Create;
begin
  FEnabled := True;
  FMethods := TList<TMethod>.Create();
  FMethods.OnNotify := InternalNotify;
end;

destructor TEvent.Destroy;
begin
  FMethods.Free();
  ReleaseMethodPointer(FInternalDispatcher);
  inherited;
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
        FInvokableType.CallingConvention, i, LOffset);
    end;

    for LMethod in FMethods do
    begin
      LArgs[0] := TValue.From<TObject>(LMethod.Data);
      // workaround for incorrect type guess in Rtti.pas
      TValueData(LArgs[0]).FTypeInfo := TypeInfo(TObject);
      Rtti.Invoke(LMethod.Code, LArgs, FInvokableType.CallingConvention, nil);
    end;
  end;
end;
{$IFEND}

procedure TEvent.InternalNotify(Sender: TObject; const Item: TMethod;
  Action: TCollectionNotification);
begin
  if Assigned(Item.Data) and not IsValid(Item.Data) then
  begin
    case Action of
      cnAdded: IInterface(Item.Data)._AddRef();
      cnRemoved: IInterface(Item.Data)._Release();
    end;
  end;
end;

procedure TEvent.Add(const AEvent: TMethod);
var
  LMethod: TMethod;
begin
  if IndexOf(AEvent) = -1 then
  begin
    LMethod.Code := TMethod(AEvent).Code;
    LMethod.Data := TMethod(AEvent).Data;
    FMethods.Add(LMethod);
  end;
  MethodAdded(TMethod(AEvent));
end;

function TEvent.GetCount: Integer;
begin
  Result := FMethods.Count;
end;

function TEvent.GetEnabled: Boolean;
begin
  Result := FEnabled;
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

function TEvent.IndexOfInstance(const AInstance: TObject): Integer;
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

procedure TEvent.Remove(const AEvent: TMethod);
var
  i: Integer;
begin
  i := IndexOf(AEvent);
  if i > -1 then
  begin
    FMethods.Delete(i);
  end;
  MethodRemoved(TMethod(AEvent));
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

procedure TEvent.SetDispatcher(var AMethod: TMethod;
  ATypeData: PTypeData);
begin
  if Assigned(FInternalDispatcher.Code)
    and Assigned(FInternalDispatcher.Data) then
  begin
    ReleaseMethodPointer(FInternalDispatcher);
  end;
  FInternalDispatcher := CreateMethodPointer(InternalInvoke, ATypeData);
  AMethod := FInternalDispatcher;
end;

procedure TEvent.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

{ TEventHandler<T> }

constructor TEvent<T>.Create(AOwner: TComponent);
var
  MethInfo: PTypeInfo;
  TypeData: PTypeData;
begin
  MethInfo := TypeInfo(T);
  TypeData := GetTypeData(MethInfo);
{$IF CompilerVersion > 22}
  FInvokableType := Context.GetType(MethInfo) as TRttiInvokableType;
  FParameters := FInvokableType.GetParameters();
{$IFEND}
  inherited Create();
  Assert(MethInfo.Kind = tkMethod, 'T must be a method pointer type');
  SetEventDispatcher(FInvoke, TypeData);
  FNotificationHandler := TNotificationHandler<TEvent<T>>.Create(Self, Notification);
  if Assigned(AOwner) then
  begin
    FOwner := AOwner;
    FOwner.FreeNotification(FNotificationHandler);
  end;
end;

constructor TEvent<T>.Create(AOwner: TComponent; AEvents: array of T);
var
  LEvent: T;
begin
  Create(AOwner);
  for LEvent in AEvents do
  begin
    Add(LEvent);
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

destructor TEvent<T>.Destroy;
begin
  FNotificationHandler.Free();
  inherited;
end;

procedure TEvent<T>.Add(AEvent: T);
begin
  inherited Add(TMethod(Pointer(@AEvent)^));
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

function TEvent<T>.GetInvoke: T;
begin
  Result := FInvoke;
end;

function TEvent<T>.GetInvokeBase: TMethod;
begin
  Result := TMethod(Pointer(@FInvoke)^);
end;

function TEvent<T>.GetOnChanged: TNotifyEvent;
begin
  Result := FOnChanged;
end;

function TEvent<T>.IndexOf(AEvent: T): Integer;
begin
  Result := inherited IndexOf(TMethod(Pointer(@AEvent)^));
end;

procedure TEvent<T>.MethodAdded(const AMethod: TMethod);
begin
  inherited;
  if IsValid(AMethod.Data) and (TObject(AMethod.Data) is TComponent) then
  begin
    FNotificationHandler.FreeNotification(TComponent(AMethod.Data));
  end;
end;

procedure TEvent<T>.MethodRemoved(const AMethod: TMethod);
begin
  inherited;
  if IsValid(AMethod.Data) and (TObject(AMethod.Data) is TComponent)
    and (IndexOfInstance(TObject(AMethod.Data)) < 0) then
  begin
    FNotificationHandler.RemoveFreeNotification(TComponent(AMethod.Data));
  end;
end;

procedure TEvent<T>.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    RemoveInstanceReferences(AComponent);
    if (AComponent = FOwner) and (RefCount = 0) then
    begin
      Free();
    end;
  end;
end;

procedure TEvent<T>.Remove(AEvent: T);
begin
  inherited Remove(TMethod(Pointer(@AEvent)^));
end;

procedure TEvent<T>.Remove<TDelegate>(ADelegate: TDelegate);
var
  LEvent: T;
begin
  MethodReferenceToMethodPointer(ADelegate, LEvent);
  Remove(LEvent);
end;

procedure TEvent<T>.SetEventDispatcher(var ADispatcher: T;
  ATypeData: PTypeData);
var
  LMethod: TMethod;
begin
  LMethod := TMethod(Pointer(@ADispatcher)^);
  inherited SetDispatcher(LMethod, ATypeData);
  TMethod(Pointer(@ADispatcher)^) := LMethod;
end;

procedure TEvent<T>.SetOnChanged(const Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

{ TEvent<T> }

constructor Event<T>.Create(AEventHandler: IEvent<T>);
begin
  FEventHandler := AEventHandler;
  FInitialized := Assigned(FEventHandler);
end;

procedure Event<T>.Add(AEvent: T);
var
  LEventHandler: IEvent<T>;
begin
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    LEventHandler.Add(AEvent);
  end;
end;

function Event<T>.GetCount: Integer;
var
  LEventHandler: IEvent<T>;
begin
  Result := 0;
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    Result := LEventHandler.Count;
  end;
end;

function Event<T>.GetEnabled: Boolean;
var
  LEventHandler: IEvent<T>;
begin
  Result := False;
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    Result := LEventHandler.Enabled;
  end;
end;

function Event<T>.GetEventHandler: IEvent<T>;
begin
  if not FInitialized then
  begin
    FEventHandler := TEvent<T>.Create(nil);
    FInitialized := True;
  end;
  Result := FEventHandler;
end;

function Event<T>.GetInvoke: T;
var
  LEventHandler: IEvent<T>;
begin
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    Result := LEventHandler.Invoke;
  end;
end;

function Event<T>.GetOnChanged: TNotifyEvent;
var
  LEventHandler: IEvent<T>;
begin
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    Result := LEventHandler.OnChanged;
  end;
end;

procedure Event<T>.Remove(AEvent: T);
var
  LEventHandler: IEvent<T>;
begin
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    LEventHandler.Remove(AEvent);
  end;
end;

procedure Event<T>.SetEnabled(const Value: Boolean);
var
  LEventHandler: IEvent<T>;
begin
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    LEventHandler.Enabled := Value;
  end;
end;

procedure Event<T>.SetOnChanged(const Value: TNotifyEvent);
var
  LEventHandler: IEvent<T>;
begin
  LEventHandler := EventHandler;
  if Assigned(LEventHandler) then
  begin
    LEventHandler.OnChanged := Value;
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
