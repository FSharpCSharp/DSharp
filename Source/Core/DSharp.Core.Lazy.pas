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

unit DSharp.Core.Lazy;

interface

uses
{$IF COMPILERVERSION > 21}
  DSharp.Core.Dynamics,
{$IFEND}
  DSharp.Core.Reflection,
  Rtti,
  SysUtils,
  TypInfo;

type
  ILazy<T> = interface(TFunc<T>)
    ['{78D1D1AA-ED7E-49B1-9DA0-402C4FA5036D}']
    function IsValueCreated: Boolean;
    property Value: T read Invoke;
  end;

{$IF COMPILERVERSION > 21}
  TLazy<T> = class(TVirtualInterface, ILazy<T>)
{$ELSE}
  TLazy<T> = class(TInterfacedObject, ILazy<T>, IInterface)
{$IFEND}
  private
    FIsValueCreated: Boolean;
    FValue: T;
    FValueFactory: TFunc<T>;
    procedure Initialize;
{$IF COMPILERVERSION > 21}
    procedure InternalInvoke(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue);
{$IFEND}
    function Invoke: T;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; {$IF COMPILERVERSION > 21}override; {$IFEND}stdcall;
  public
    constructor Create(ValueFactory: TFunc<T>); overload;
    destructor Destroy; override;

    function IsValueCreated: Boolean;
    property Value: T read Invoke;
  end;

  TLazy = class(TLazy<IInterface>)
  public
    constructor Create(ValueFactory: TFunc<IInterface>; TypeInfo: PTypeInfo);
  end;

  Lazy<T> = record
  strict private
    FLazy: ILazy<T>;
    function GetValue: T;
  public
    class constructor Create;
    property Value: T read GetValue;
    class operator Implicit(const Value: TFunc<T>): Lazy<T>; overload;
    class operator Implicit(const Value: Lazy<T>): T; overload;
  end;

const
  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';

implementation

{ TLazy<T> }

constructor TLazy<T>.Create(ValueFactory: TFunc<T>);
begin
{$IF COMPILERVERSION > 21}
  inherited Create(TypeInfo(T), InternalInvoke);
{$IFEND}
  FValueFactory := ValueFactory;
end;

destructor TLazy<T>.Destroy;
var
  LTypeInfo: PTypeInfo;
begin
  if FIsValueCreated then
  begin
    LTypeInfo := TypeInfo(T);
    if LTypeInfo.Kind = tkClass then
    begin
      TObject(PPointer(@FValue)^).Free();
    end;
  end;
  inherited;
end;

procedure TLazy<T>.Initialize;
begin
  if not FIsValueCreated then
  begin
    FValue := FValueFactory();
    FIsValueCreated := True;
{$IF COMPILERVERSION > 21}
    if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
    begin
      Instance := IInterface(PPointer(@FValue)^);
    end;
{$IFEND}
  end;
end;

{$IF COMPILERVERSION > 21}
procedure TLazy<T>.InternalInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  i: Integer;
  LArgs: TArray<TValue>;
  LParams: TArray<TRttiParameter>;
begin
  Initialize();
  LParams := Method.GetParameters;
  SetLength(LArgs, Pred(Length(Args)));
  for i := 0 to Pred(Length(LArgs)) do
  begin
    LArgs[i] := Args[i + 1];
  end;
  Result := Method.Invoke(TValue.From<T>(FValue), LArgs);
end;
{$IFEND}

function TLazy<T>.Invoke: T;
begin
{$IF COMPILERVERSION > 21}
  if PTypeInfo(TypeInfo(T)).Kind = tkInterface then
  begin
    Supports(Self, InterfaceID, Result);
  end
  else
{$IFEND}
  begin
    Initialize();
    Result := FValue;
  end;
end;

function TLazy<T>.IsValueCreated: Boolean;
begin
  Result := FIsValueCreated;
end;

function TLazy<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, ObjCastGUID) then
  begin
    Initialize;
  end;
  Result := inherited;
end;

{ TLazy }

constructor TLazy.Create(ValueFactory: TFunc<IInterface>; TypeInfo: PTypeInfo);
begin
{$IF COMPILERVERSION > 21}
  inherited Create(TypeInfo, InternalInvoke);
{$ELSE}
  inherited Create(ValueFactory);
{$IFEND}
  FValueFactory := ValueFactory;
end;

{ Lazy<T> }

class constructor Lazy<T>.Create;
begin
  // cause RTTI to get loaded for T
  GetRttiType(TypeInfo(T));
end;

function Lazy<T>.GetValue: T;
begin
  Result := FLazy();
end;

class operator Lazy<T>.Implicit(const Value: TFunc<T>): Lazy<T>;
begin
  Result.FLazy := TLazy<T>.Create(Value);
end;

class operator Lazy<T>.Implicit(const Value: Lazy<T>): T;
begin
  Result := Value.Value;
end;

end.
