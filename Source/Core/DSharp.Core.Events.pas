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
  TypInfo;

type
  TEventHandler = class abstract(TInterfacedObject)
  strict private
    FInternalDispatcher: TMethod;
    FMethods: TList<TMethod>;
    procedure InternalInvoke(Params: PParameters; StackSize: Integer);
    procedure InternalNotify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification);
  strict protected
    type
      TEvent = procedure of object;
    procedure MethodAdded(const AMethod: TMethod); virtual; abstract;
    procedure MethodRemoved(const AMethod: TMethod); virtual; abstract;
    procedure Add(const AEvent: TEvent);
    function IndexOf(const AEvent: TEvent): Integer;
    function IndexOfInstance(const AInstance: TObject): Integer;
    procedure Remove(const AEvent: TEvent);
    procedure RemoveInstanceReferences(const AInstance: TObject);
    procedure SetDispatcher(var AMethod: TMethod; ATypeData: PTypeData);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  IEventHandler<T> = interface
    function GetInvoke: T;
    procedure Add(AEvent: T);
    procedure Remove(AEvent: T);
    property Invoke: T read GetInvoke;
  end;

  TEventHandler<T> = class sealed(TEventHandler, IEventHandler<T>)
  strict private
    FInvoke: T;
    FNotificationHandler: TNotificationHandler<TEventHandler<T>>;
    FOwner: TComponent;
    function GetInvoke: T;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure SetEventDispatcher(var ADispatcher: T; ATypeData: PTypeData);
  strict protected
    procedure MethodAdded(const AMethod: TMethod); override;
    procedure MethodRemoved(const AMethod: TMethod); override;
  public
    constructor Create(AOwner: TComponent); overload;
    constructor Create(AOwner: TComponent; AEvents: array of T); overload;
{$IFNDEF VER210}
    class function Create<TDelegate>(AOwner: TComponent;
      ADelegates: array of TDelegate): TEventHandler<T>; overload;
{$ENDIF}
    class function Create<TDelegate>(AOwner: TComponent;
      ADelegates: TArray<TDelegate>): TEventHandler<T>; overload;
    destructor Destroy; override;
    procedure Add(AEvent: T); overload;
    procedure Add<TDelegate>(ADelegate: TDelegate); overload;
    procedure Remove(AEvent: T); overload;
    procedure Remove<TDelegate>(ADelegate: TDelegate); overload;
    function IndexOf(AEvent: T): Integer;
    property Invoke: T read GetInvoke;
    property Owner: TComponent read FOwner;
  end;

  TEvent<T> = record
  strict private
    FEventHandler: IEventHandler<T>;
    function GetInvoke: T;
    function GetEventHandler: IEventHandler<T>;
  public
    constructor Create(AEventHandler: IEventHandler<T>);

    procedure Add(AEvent: T);
    procedure Remove(AEvent: T);
    property EventHandler: IEventHandler<T> read GetEventHandler;
    property Invoke: T read GetInvoke;

    class operator Implicit(const AValue: TEvent<T>): T;
    class operator Implicit(const AValue: IEventHandler<T>): TEvent<T>;
  end;

function IsValid(AObject: TObject): Boolean;
procedure MethodReferenceToMethodPointer(const AMethodReference; var AMethodPointer);

implementation

function IsValid(AObject: TObject): Boolean;
begin
  Result := False;
  if Assigned(AObject) then
  try
    if NativeInt(Pointer(PPointer(AObject)^)) > $FFFF then  // "hotfix" to prevent some access violations (no clue if this works) :)
      Result := Pointer(PPointer(AObject)^) =
        Pointer(Pointer(Cardinal(PPointer(AObject)^) + Cardinal(vmtSelfPtr))^);
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

{ TEventHandler }

constructor TEventHandler.Create;
begin
  FMethods := TList<TMethod>.Create();
  FMethods.OnNotify := InternalNotify;
end;

destructor TEventHandler.Destroy;
begin
  FMethods.Free();
  ReleaseMethodPointer(FInternalDispatcher);
  inherited;
end;

procedure TEventHandler.InternalInvoke(Params: PParameters; StackSize: Integer);
const
  PtrSize = SizeOf(Pointer);
var
  LMethod: TMethod;
begin
  for LMethod in FMethods do
  begin
    // "Push" parameters on stack
    if StackSize > 0 then
    asm
      // Put StackSize as third parameter
      MOV ECX,StackSize
      // stack address alignment
      ADD ECX,PtrSize-1
      AND ECX,NOT(PtrSize-1)
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

procedure TEventHandler.InternalNotify(Sender: TObject; const Item: TMethod;
  Action: TCollectionNotification);
begin
  if not IsValid(Item.Data) then
  begin
    case Action of
      cnAdded: IInterface(Item.Data)._AddRef();
      cnRemoved: IInterface(Item.Data)._Release();
    end;
  end;
end;

procedure TEventHandler.Add(const AEvent: TEvent);
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

function TEventHandler.IndexOf(const AEvent: TEvent): Integer;
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

function TEventHandler.IndexOfInstance(const AInstance: TObject): Integer;
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

procedure TEventHandler.Remove(const AEvent: TEvent);
var
  i: Integer;
begin
  i := IndexOf(AEvent);
  if i > -1 then
  begin
    FMethods.Delete(i)
  end;
  MethodAdded(TMethod(AEvent));
end;

procedure TEventHandler.RemoveInstanceReferences(const AInstance: TObject);
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

procedure TEventHandler.SetDispatcher(var AMethod: TMethod;
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

{ TEventHandler<T> }

constructor TEventHandler<T>.Create(AOwner: TComponent);
var
  MethInfo: PTypeInfo;
  TypeData: PTypeData;
begin
  MethInfo := TypeInfo(T);
  TypeData := GetTypeData(MethInfo);
  inherited Create();
  Assert(MethInfo.Kind = tkMethod, 'T must be a method pointer type');
  SetEventDispatcher(FInvoke, TypeData);
  FNotificationHandler := TNotificationHandler<TEventHandler<T>>.Create(Self, Notification);
  if Assigned(AOwner) then
  begin
    FOwner := AOwner;
    FOwner.FreeNotification(FNotificationHandler);
  end;
end;

constructor TEventHandler<T>.Create(AOwner: TComponent; AEvents: array of T);
var
  LEvent: T;
begin
  Create(AOwner);
  for LEvent in AEvents do
  begin
    Add(LEvent);
  end;
end;

{$IFNDEF VER210}
class function TEventHandler<T>.Create<TDelegate>(AOwner: TComponent;
  ADelegates: array of TDelegate): TEventHandler<T>;
var
  LDelegate: TDelegate;
begin
  Result := Create(AOwner);
  for LDelegate in ADelegates do
  begin
    Result.Add<TDelegate>(LDelegate);
  end;
end;
{$ENDIF}

class function TEventHandler<T>.Create<TDelegate>(AOwner: TComponent;
  ADelegates: TArray<TDelegate>): TEventHandler<T>;
var
  LDelegate: TDelegate;
begin
  Result := Create(AOwner);
  for LDelegate in ADelegates do
  begin
    Result.Add<TDelegate>(LDelegate);
  end;
end;

destructor TEventHandler<T>.Destroy;
begin
  FNotificationHandler.Free();
  inherited;
end;

procedure TEventHandler<T>.Add(AEvent: T);
begin
  inherited Add(TEvent(Pointer(@AEvent)^));
end;

procedure TEventHandler<T>.Add<TDelegate>(ADelegate: TDelegate);
var
  LEvent: T;
  LTypeInfo: PTypeInfo;
  LTypeData: PTypeData;
//  LContext: TRttiContext;
//  LMethod: TRttiMethod;
//  LParams: TArray<TRttiParameter>;
begin
  LTypeInfo := TypeInfo(TDelegate);
  Assert(LTypeInfo.Kind = tkInterface, 'TDelegate must be a method reference');
  LTypeInfo := TypeInfo(T);
  LTypeData := GetTypeData(LTypeInfo);

//  Does not work right now because method references are missing RTTI
//  LMethod := LContext.GetType(TypeInfo(TDelegate)).GetMethod('Invoke');
//  Assert(LMethod.MethodKind = LTypeData.MethodKind, 'MethodKind does not match');
//  LParams := LMethod.GetParameters();
//  Assert(Length(LParams) = LTypeData.ParamCount, 'ParamCount does not match');
  MethodReferenceToMethodPointer(ADelegate, LEvent);
  Add(LEvent);
end;

function TEventHandler<T>.GetInvoke: T;
begin
  Result := FInvoke;
end;

function TEventHandler<T>.IndexOf(AEvent: T): Integer;
begin
  Result := inherited IndexOf(TEvent(Pointer(@AEvent)^));
end;

procedure TEventHandler<T>.MethodAdded(const AMethod: TMethod);
begin
  inherited;
  if IsValid(AMethod.Data) and (TObject(AMethod.Data) is TComponent) then
  begin
    FNotificationHandler.FreeNotification(TComponent(AMethod.Data));
  end;
end;

procedure TEventHandler<T>.MethodRemoved(const AMethod: TMethod);
begin
  inherited;
  if IsValid(AMethod.Data) and (TObject(AMethod.Data) is TComponent)
    and (IndexOfInstance(TObject(AMethod.Data)) < 0) then
  begin
    FNotificationHandler.RemoveFreeNotification(TComponent(AMethod.Data));
  end;
end;

procedure TEventHandler<T>.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    RemoveInstanceReferences(AComponent);
    if AComponent = FOwner then
    begin
      Free();
    end;
  end;
end;

procedure TEventHandler<T>.Remove(AEvent: T);
begin
  inherited Remove(TEvent(Pointer(@AEvent)^));
end;

procedure TEventHandler<T>.Remove<TDelegate>(ADelegate: TDelegate);
var
  LEvent: T;
begin
  MethodReferenceToMethodPointer(ADelegate, LEvent);
  Remove(LEvent);
end;

procedure TEventHandler<T>.SetEventDispatcher(var ADispatcher: T;
  ATypeData: PTypeData);
var
  LMethod: TMethod;
begin
  LMethod := TMethod(Pointer(@ADispatcher)^);
  inherited SetDispatcher(LMethod, ATypeData);
  TMethod(Pointer(@ADispatcher)^) := LMethod;
end;

{ TEvent<T> }

constructor TEvent<T>.Create(AEventHandler: IEventHandler<T>);
begin
  FEventHandler := AEventHandler;
end;

procedure TEvent<T>.Add(AEvent: T);
begin
  EventHandler.Add(AEvent);
end;

function TEvent<T>.GetEventHandler: IEventHandler<T>;
begin
  if not Assigned(FEventHandler) then
  begin
    FEventHandler := TEventHandler<T>.Create(nil);
  end;
  Result := FEventHandler;
end;

function TEvent<T>.GetInvoke: T;
begin
  Result := EventHandler.Invoke;
end;

procedure TEvent<T>.Remove(AEvent: T);
begin
  EventHandler.Remove(AEvent);
end;

class operator TEvent<T>.Implicit(const AValue: TEvent<T>): T;
begin
  Result := AValue.EventHandler.Invoke;
end;

class operator TEvent<T>.Implicit(const AValue: IEventHandler<T>): TEvent<T>;
begin
  Result := TEvent<T>.Create(AValue);
end;

end.
