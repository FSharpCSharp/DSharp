(*
  Copyright (c) 2011-2013, Stefan Glienke
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

unit DSharp.Testing.Mock.Internals;

interface

uses
  Classes,
  DSharp.Core.Times,
  DSharp.Interception,
  DSharp.Testing.Mock.Expectation,
  DSharp.Testing.Mock.Interfaces,
  Generics.Collections,
  Rtti,
  SysUtils,
  TypInfo;

type
  TMockState = (msDefining, msExecuting);
  TMockType = (mtUndefined, mtObject, mtInterface);

  TMock = class(TInterfacedObject, IMock)
  private
    FCurrentExpectation: TExpectation;
    FCurrentSequence: ISequence;
    FExpectations: TObjectList<TExpectation>;
    FMode: TMockMode;
    FState: TMockState;
    FTypeInfo: PTypeInfo;
    FProxy: TValue;
    class var FOnVerify: TNotifyEvent;
    function GetInstance: TValue;
  protected
    function GetMode: TMockMode;
    procedure SetExpectedTimes(const Value: Times);
    procedure SetMode(const Value: TMockMode);
  public
    constructor Create(typeInfo: PTypeInfo);
    destructor Destroy; override;

    function HasExpectation(Method: TRttiMethod): Boolean;
    function FindExpectation(Method: TRttiMethod;
      const Arguments: TArray<TValue>): TExpectation;
    procedure Verify;

    property CurrentExpectation: TExpectation
      read FCurrentExpectation write FCurrentExpectation;
    property Mode: TMockMode read GetMode write SetMode;
    property State: TMockState read FState write FState;

    class property OnVerify: TNotifyEvent read FOnVerify write FOnVerify;
  end;

  TMock<T> = class(TMock, IMock<T>, ISetup<T>,
    IExpect<T>, IExpectInSequence<T>, IWhen<T>)
  private
    function GetInstance: T;
    function Setup: ISetup<T>;
  public
    constructor Create;

    // IExpect<T>
    function Any: IWhen<T>;
    function AtLeast(const Count: Cardinal): IWhen<T>;
    function AtLeastOnce: IWhen<T>;
    function AtMost(const Count: Cardinal): IWhen<T>;
    function AtMostOnce: IWhen<T>;
    function Between(const LowValue, HighValue: Cardinal): IWhen<T>;
    function Exactly(const Count: Cardinal): IWhen<T>;
    function Never: IWhen<T>;
    function Once: IWhen<T>;

    // IExpectInSequence<T>
    function InSequence(Sequence: ISequence): IExpect<T>;

    // IWhen<T>
    function WhenCalling: T;
    function WhenCallingWithAnyArguments: T;

    // ISetup<T>
    function WillExecute(const Action: TMockAction): IExpectInSequence<T>;
    function WillRaise(const E: TFunc<Exception>): IExpectInSequence<T>;
    function WillReturn(const Value: TValue): IExpectInSequence<T>;

    property Instance: T read GetInstance;
  end;

  TMockBehavior = class(TInterfacedObject, IInterceptionBehavior)
  private
    FMock: TMock;
    function Invoke(Input: IMethodInvocation;
      GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
    function WillExecute: Boolean;
  public
    constructor Create(Mock: TMock);
  end;

implementation

uses
  DSharp.Core.Reflection;

const
  CUnexpectedInvocation = 'unexpected invocation: %s';
  CUnperformedExpectation = 'unperformed expectation: %s';

{ TMock }

constructor TMock.Create(typeInfo: PTypeInfo);
begin
  inherited Create;
  FExpectations := TObjectList<TExpectation>.Create(True);
  FTypeInfo := typeInfo;
  FProxy := TIntercept.ThroughProxy(FProxy, fTypeInfo, nil, [TMockBehavior.Create(Self)]);
  FState := msExecuting;
end;

destructor TMock.Destroy;
begin
  FExpectations.Free;
  inherited;
end;

function TMock.FindExpectation(Method: TRttiMethod;
  const Arguments: TArray<TValue>): TExpectation;
var
  LExpectation: TExpectation;
begin
  Result := nil;
  for LExpectation in FExpectations do
  begin
    // only get expectation if corrent method
    if (LExpectation.Method = Method)
      // and argument values match if they need to
      and (LExpectation.AnyArguments or TValue.Equals(LExpectation.Arguments, Arguments)) then
    begin
      Result := LExpectation;
      // if expectation can be invoked it can be used
      if LExpectation.AllowsInvocation then
        Break;
    end;
  end;

  if not Assigned(Result) then
    raise EMockException.CreateFmt(CUnexpectedInvocation, [Method.Format(Arguments, True)])
  else
    if not Result.AllowsInvocation then
      raise EMockException.CreateFmt(CUnexpectedInvocation, [Result.ToString]);
end;

function TMock.GetInstance: TValue;
begin
  Result := FProxy;
end;

function TMock.GetMode: TMockMode;
begin
  Result := FMode;
end;

function TMock.HasExpectation(Method: TRttiMethod): Boolean;
var
  LExpectation: TExpectation;
begin
  Result := False;
  for LExpectation in FExpectations do
  begin
    if LExpectation.Method = Method then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TMock.SetExpectedTimes(const Value: Times);
begin
  if Assigned(FCurrentSequence) then
  begin
    FCurrentSequence.ExpectInvocation(FCurrentExpectation, Value);
    FCurrentSequence := nil;
  end
  else
  begin
    FCurrentExpectation.ExpectedCount := Value;
  end;
end;

procedure TMock.SetMode(const Value: TMockMode);
begin
  FMode := Value;
end;

procedure TMock.Verify;
var
  LExpectation: TExpectation;
begin
  if Assigned(FOnVerify) then
    FOnVerify(Self);

  for LExpectation in FExpectations do
    if not LExpectation.IsSatisfied then
      raise EMockException.CreateFmt(CUnperformedExpectation, [LExpectation.ToString]);
end;

{ TMock<T> }

constructor TMock<T>.Create;
begin
  inherited Create(TypeInfo(T));
end;

function TMock<T>.Any: IWhen<T>;
begin
  SetExpectedTimes(Times.Any);
  Result := Self;
end;

function TMock<T>.AtLeast(const Count: Cardinal): IWhen<T>;
begin
  SetExpectedTimes(Times.AtLeast(Count));
  Result := Self;
end;

function TMock<T>.AtLeastOnce: IWhen<T>;
begin
  SetExpectedTimes(Times.AtLeastOnce);
  Result := Self;
end;

function TMock<T>.AtMost(const Count: Cardinal): IWhen<T>;
begin
  SetExpectedTimes(Times.AtMost(Count));
  Result := Self;
end;

function TMock<T>.AtMostOnce: IWhen<T>;
begin
  SetExpectedTimes(Times.AtMostOnce);
  Result := Self;
end;

function TMock<T>.Between(const LowValue, HighValue: Cardinal): IWhen<T>;
begin
  SetExpectedTimes(Times.Between(LowValue, HighValue));
  Result := Self;
end;

function TMock<T>.Exactly(const Count: Cardinal): IWhen<T>;
begin
  SetExpectedTimes(Times.Exactly(Count));
  Result := Self;
end;

function TMock<T>.Never: IWhen<T>;
begin
  SetExpectedTimes(Times.Never);
  Result := Self;
end;

function TMock<T>.Once: IWhen<T>;
begin
  SetExpectedTimes(Times.Once);
  Result := Self;
end;

function TMock<T>.Setup: ISetup<T>;
begin
  Result := Self;
end;

function TMock<T>.GetInstance: T;
begin
  Result := FProxy.AsType<T>;
end;

function TMock<T>.InSequence(Sequence: ISequence): IExpect<T>;
begin
  FCurrentSequence := Sequence;
  Result := Self;
end;

function TMock<T>.WhenCalling: T;
begin
  Result := Instance;
end;

function TMock<T>.WhenCallingWithAnyArguments: T;
begin
  FCurrentExpectation.AnyArguments := True;
  Result := Instance;
end;

function TMock<T>.WillExecute(const Action: TMockAction): IExpectInSequence<T>;
begin
  FCurrentExpectation := TExpectation.Create();
  FCurrentExpectation.Action := Action;
  Result := Self;
  FState := msDefining;
  FExpectations.Add(FCurrentExpectation);
end;

function TMock<T>.WillRaise(const E: TFunc<Exception>): IExpectInSequence<T>;
begin
  FCurrentExpectation := TExpectation.Create();
  FCurrentExpectation.Exception := E;
  Result := Self;
  FState := msDefining;
  FExpectations.Add(FCurrentExpectation);
end;

function TMock<T>.WillReturn(const Value: TValue): IExpectInSequence<T>;
begin
  FCurrentExpectation := TExpectation.Create();
  FCurrentExpectation.Result := Value;
  Result := Self;
  FState := msDefining;
  FExpectations.Add(FCurrentExpectation);
end;

{ TMockBehavior }

constructor TMockBehavior.Create(Mock: TMock);
begin
  FMock := Mock;
end;

function TMockBehavior.Invoke(Input: IMethodInvocation;
  GetNext: TFunc<TInvokeInterceptionBehaviorDelegate>): IMethodReturn;
begin
  case FMock.State of
    msDefining:
    begin
      FMock.CurrentExpectation.Method := Input.Method;
      FMock.CurrentExpectation.Arguments := Input.Arguments;
      FMock.State := msExecuting;
    end;
    msExecuting:
    begin
      // no expectation for this method defined and in stub mode
      if (FMock.Mode = Stub) and not FMock.HasExpectation(Input.Method) then
        Exit;

      FMock.CurrentExpectation := FMock.FindExpectation(Input.Method, Input.Arguments);
      Result := Input.CreateMethodReturn(
        FMock.CurrentExpectation.Execute(Input.Arguments, Input.Method.ReturnType));
    end;
  end;
end;

function TMockBehavior.WillExecute: Boolean;
begin
  Result := True;
end;

end.
