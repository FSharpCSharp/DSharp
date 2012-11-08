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

unit DSharp.Interception;

interface

uses
  DSharp.Interception.MethodImplementationInfo,
  Rtti,
  SysUtils,
  TypInfo;

type
  IMethodReturn = interface
    function GetException: Exception;
    function GetReturnValue: TValue;
    procedure SetException(const Value: Exception);
    procedure SetReturnValue(const Value: TValue);

    property Exception: Exception read GetException write SetException;
    property ReturnValue: TValue read GetReturnValue write SetReturnValue;
  end;

  IMethodInvocation = interface
    function GetArguments: TArray<TValue>;
    function GetMethod: TRttiMethod;
    function GetTarget: TValue;

    function CreateMethodReturn(const ReturnValue: TValue): IMethodReturn;
    function CreateExceptionMethodReturn(E: Exception): IMethodReturn;

    property Arguments: TArray<TValue> read GetArguments;
    property Method: TRttiMethod read GetMethod;
    property Target: TValue read GetTarget;
  end;

  TInvokeHandlerDelegate = reference to function(
    Input: IMethodInvocation;
    GetNext: TFunc<TInvokeHandlerDelegate>): IMethodReturn;

  IInvokeHandlerDelegate = interface(TInvokeHandlerDelegate)
    ['{B647E6CE-49A9-434B-92B9-7EC1BE20A654}']
  end;

  ICallHandler = interface
    ['{E583D6ED-82F2-4AC8-B899-C70ED56BCBE0}']
    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeHandlerDelegate>): IMethodReturn;
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
    property Order: Integer read GetOrder write SetOrder;
  end;

  TInvokeInterceptionBehaviorDelegate = reference to function(
    Input: IMethodInvocation;
    GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;

  IInvokeInterceptionBehaviorDelegate = interface(TInvokeInterceptionBehaviorDelegate)
    ['{5C26AD28-61E5-41EC-8975-94AB058002FA}']
  end;

  IInterceptionBehavior = interface
    ['{B55F8EC2-EE17-4907-967F-B6BD08004A6A}']
    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
//    function GetRequiredInterfaces: TArray<PTypeInfo>;
    function WillExecute: Boolean;
  end;

  IInterceptor = interface
    ['{D752310D-7E06-496C-9A90-9E3DCCDA1588}']
    function CanIntercept(TypeInfo: PTypeInfo): Boolean;
    function GetInterceptableMethods(InterceptedType: PTypeInfo;
      ImplementationType: PTypeInfo): TArray<TMethodImplementationInfo>;
  end;

  IInterceptingProxy = interface
    ['{0E9A0437-3D7A-4C7E-B445-7E2A5AB52F12}']
    procedure AddInterceptionBehavior(Interceptor: IInterceptionBehavior);
  end;

  IInstanceInterceptor = interface(IInterceptor)
    ['{0D9F752E-E156-4084-9132-1C349928CD1C}']
    function CreateProxy(TypeInfo: PTypeInfo; Instance: Pointer): IInterceptingProxy;
  end;

  TCurrentInterceptionRequest = record
  private
    FInterceptor: IInterceptor;
    FTypeToIntercept: PTypeInfo;
    FImplementationType: PTypeInfo;
  public
    constructor Create(Interceptor: IInterceptor; TypeToIntercept: PTypeInfo;
      ImplementationType: PTypeInfo);

    property Interceptor: IInterceptor read FInterceptor write FInterceptor;
    property TypeToIntercept: PTypeInfo read FTypeToIntercept write FTypeToIntercept;
    property ImplementationType: PTypeInfo read FImplementationType write FImplementationType;
  end;

  IInjectionPolicy = interface
    ['{A71743E2-40B8-4BCE-BDEB-B5324B72BBC0}']
    function GetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>;
    function Matches(Member: TMethodImplementationInfo): Boolean;
  end;

  IMatchingRule = interface
    ['{3444B2DF-067E-4E41-A835-FB11902A45A6}']
    function Matches(Member: TRttiMethod): Boolean;
  end;

  ApplyNoPoliciesAttribute = class(TCustomAttribute);

  HandlerAttribute = class(TCustomAttribute)
  private
    FOrder: Integer;
  public
    function CreateHandler: ICallHandler; virtual; abstract;
    property Order: Integer read FOrder write FOrder;
  end;

  TIntercept = record
    class function ThroughProxy(InterceptedType: PTypeInfo; Target: Pointer;
      Interceptor: IInstanceInterceptor;
      InterceptionBehaviors: array of IInterceptionBehavior): IInterceptingProxy; overload; static;

    class function ThroughProxy(const Target: TValue; TypeInfo: PTypeInfo;
      Interceptor: IInstanceInterceptor;
      InterceptionBehaviors: array of IInterceptionBehavior): TValue; overload; static;
    class function ThroughProxy<T>(Target: T; Interceptor: IInstanceInterceptor;
      InterceptionBehaviors: array of IInterceptionBehavior): T; overload; static;

    class function ThroughProxyByAttributes(const Target: TValue; TypeInfo: PTypeInfo;
      Interceptor: IInstanceInterceptor = nil): TValue; overload; static;
    class function ThroughProxyByAttributes<T>(Target: T;
      Interceptor: IInstanceInterceptor = nil): T; overload; static;
  end;

implementation

uses
  DSharp.Interception.AttributeDrivenPolicy,
  DSharp.Interception.ClassInterceptor,
  DSharp.Interception.ClassProxy,
  DSharp.Interception.InterfaceInterceptor,
  DSharp.Interception.PolicyInjectionBehavior;

resourcestring
  SInterceptionNotSupported = 'The type %0:s is not interceptable.';

{ TCurrentInterceptionRequest }

constructor TCurrentInterceptionRequest.Create(Interceptor: IInterceptor;
  TypeToIntercept, ImplementationType: PTypeInfo);
begin
  FInterceptor := Interceptor;
  FTypeToIntercept := TypeToIntercept;
  FImplementationType := ImplementationType;
end;

{ TIntercept }

class function TIntercept.ThroughProxy(InterceptedType: PTypeInfo;
  Target: Pointer; Interceptor: IInstanceInterceptor;
  InterceptionBehaviors: array of IInterceptionBehavior): IInterceptingProxy;
var
  i: Integer;
begin
  if not Interceptor.CanIntercept(InterceptedType) then
    raise EArgumentException.CreateResFmt(@SInterceptionNotSupported, [InterceptedType.Name]);

  Result := Interceptor.CreateProxy(InterceptedType, Target);
  for i := 0 to High(InterceptionBehaviors) do
  begin
    if InterceptionBehaviors[i].WillExecute then
    begin
      Result.AddInterceptionBehavior(InterceptionBehaviors[i]);
    end;
  end;
end;

class function TIntercept.ThroughProxy(const Target: TValue; TypeInfo: PTypeInfo;
  Interceptor: IInstanceInterceptor;
  InterceptionBehaviors: array of IInterceptionBehavior): TValue;
var
  proxy: IInterceptingProxy;
  instance: Pointer;
begin
  instance := PPointer(Target.GetReferenceToRawData)^;
  proxy := ThroughProxy(TypeInfo, instance, Interceptor, InterceptionBehaviors);
  case TypeInfo.Kind of
    tkClass: instance := (proxy as TClassProxy).Instance;
    tkInterface: proxy.QueryInterface(GetTypeData(TypeInfo).Guid, instance);
  end;
  TValue.Make(@instance, TypeInfo, Result);
end;

class function TIntercept.ThroughProxy<T>(Target: T;
  Interceptor: IInstanceInterceptor;
  InterceptionBehaviors: array of IInterceptionBehavior): T;
begin
  Result := ThroughProxy(TValue.From<T>(Target), TypeInfo(T),
    Interceptor, InterceptionBehaviors).AsType<T>;
end;

class function TIntercept.ThroughProxyByAttributes(const Target: TValue;
  TypeInfo: PTypeInfo; Interceptor: IInstanceInterceptor): TValue;
begin
  if not Assigned(Interceptor) then
  begin
    case TypeInfo.Kind of
      tkClass: Interceptor := TClassInterceptor.Create;
      tkInterface: Interceptor := TInterfaceInterceptor.Create;
    end;
  end;

  Result := ThroughProxy(Target, TypeInfo, Interceptor, [
    TPolicyInjectionBehavior.Create(TCurrentInterceptionRequest.Create(
    Interceptor, TypeInfo, nil), [TAttributeDrivenPolicy.Create])]);
end;

class function TIntercept.ThroughProxyByAttributes<T>(Target: T;
  Interceptor: IInstanceInterceptor): T;
begin
  Result := ThroughProxyByAttributes(TValue.From<T>(Target), TypeInfo(T), Interceptor).AsType<T>;
end;

end.
