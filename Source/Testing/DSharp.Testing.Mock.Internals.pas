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

unit DSharp.Testing.Mock.Internals;

interface

uses
  DSharp.Core.Times,
{$IF COMPILERVERSION < 23}
  DSharp.Core.VirtualInterface,
{$IFEND}
  DSharp.Testing.Mock.Expectation,
  DSharp.Testing.Mock.Interfaces,
  Generics.Collections,
  Rtti,
  SysUtils;

type
  TMockState = (msDefining, msExecuting);
  TMockType = (mtUndefined, mtObject, mtInterface);

  TMock<T> = class(TVirtualInterface,
    IMock<T>, IExpect<T>, IExpectInSequence<T>, IWhen<T>)
  private
    FCurrentExpectation: TExpectation;
    FCurrentSequence: ISequence;
    FExpectations: TObjectList<TExpectation>;
    FInstance: Pointer;
    FInterceptor: TVirtualMethodInterceptor;
    FMode: TMockMode;
    FState: TMockState;
    FType: TMockType;
    procedure CreateInterfaceMock(AType: TRttiType);
    procedure CreateObjectMock(AType: TRttiType);
    procedure DestroyInterfaceMock;
    procedure DestroyObjectMock;
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoMethodCall(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue);
    function FindExpectation(Method: TRttiMethod;
      Arguments: TArray<TValue>): TExpectation;
    function GetInstance: T;
    function GetMode: TMockMode;
    function HasExpectation(Method: TRttiMethod): Boolean;
    procedure SetExpectedTimes(const Times: Times);
    procedure SetMode(const Value: TMockMode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Verify;

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

    // IMock<T>
    function WillExecute(const Action: TMockAction): IExpectInSequence<T>;
    function WillRaise(const E: TFunc<Exception>): IExpectInSequence<T>;
    function WillReturn(const Value: TValue): IExpectInSequence<T>;

    property Instance: T read GetInstance;
  end;

const
  CCreationError = 'unable to create mock for type: %s (contains no methods or typeinfo)';
  CUnmetExpectations = 'not all expected invocations were performed';
  CUnexpectedInvocation = 'unexpected invocation of: %s';

implementation

uses
  DSharp.Core.Reflection,
  RTLConsts,
  TypInfo;

{ TMock<T> }

constructor TMock<T>.Create;
var
  LType: TRttiType;
begin
  FExpectations := TObjectList<TExpectation>.Create(True);
  FState := msExecuting;
  LType := GetRttiType(System.TypeInfo(T));
  if LType is TRttiInstanceType then
  begin
    CreateObjectMock(LType);
  end else
  if (LType is TRttiInterfaceType)
    and (LType.MethodCount > 0) then
  begin
    CreateInterfaceMock(LType);
  end else
  begin
    raise EMockException.CreateFmt(CCreationError, [LType.Name]);
  end;
end;

destructor TMock<T>.Destroy;
begin
  FExpectations.Free();
  case FType of
    mtObject: DestroyObjectMock();
    mtInterface: DestroyInterfaceMock();
  end;
end;

procedure TMock<T>.CreateInterfaceMock(AType: TRttiType);
begin
  inherited Create(AType.Handle, DoMethodCall);
  Supports(Self, TRttiInterfaceType(AType).GUID, FInstance);
  FType := mtInterface;
end;

procedure TMock<T>.CreateObjectMock(AType: TRttiType);
begin
  FInstance := AType.GetStandardConstructor().Invoke(
    AType.AsInstance.MetaclassType, []).AsObject();
  FInterceptor := TVirtualMethodInterceptor.Create(AType.AsInstance.MetaclassType);
  FInterceptor.Proxify(PObject(@FInstance)^);
  FInterceptor.OnBefore := DoBefore;
  FType := mtObject;
end;

procedure TMock<T>.DestroyInterfaceMock;
begin
  inherited Destroy;
end;

procedure TMock<T>.DestroyObjectMock;
begin
  PObject(@FInstance)^.Free();
  FInterceptor.Free();
end;

procedure TMock<T>.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
begin
  if Method.Parent.AsInstance.MetaclassType <> TObject then
  begin
    DoInvoke := False;

    DoMethodCall(Method, Args, Result);
  end;
end;

procedure TMock<T>.DoMethodCall(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  LSequence: TList<TExpectation>;
begin
  Result := TValue.Empty;

  case FState of
    msDefining:
    begin
      FCurrentExpectation.Method := Method;
      FCurrentExpectation.Arguments := Args;
      FState := msExecuting;
    end;
    msExecuting:
    begin
      // no expectation for this method defined and in stub mode
      if (FMode = Stub) and not HasExpectation(Method) then
        Exit;

      FCurrentExpectation := FindExpectation(Method, Args);
      if Assigned(FCurrentExpectation) then
      begin
        Result := FCurrentExpectation.Execute(Args, Method.ReturnType);
      end
      else
      begin
        raise EMockException.CreateFmt(CUnexpectedInvocation, [
          Method.Format(Args, True)]);
      end;
    end;
  end;
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

function TMock<T>.FindExpectation(Method: TRttiMethod;
  Arguments: TArray<TValue>): TExpectation;
var
  LExpectation: TExpectation;
begin
  Result := nil;
  for LExpectation in FExpectations do
  begin
    // only get expectation if corrent method
    if (LExpectation.Method = Method)
      // and argument values match if they need to
      and (LExpectation.AnyArguments or TValue.Equals(LExpectation.Arguments, Arguments))
      and LExpectation.AllowsInvocation then
    begin
      Result := LExpectation;
      Break;
    end;
  end;
end;

function TMock<T>.GetInstance: T;
begin
  PPointer(@Result)^ := FInstance;
end;

function TMock<T>.GetMode: TMockMode;
begin
  Result := FMode;
end;

function TMock<T>.HasExpectation(Method: TRttiMethod): Boolean;
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

function TMock<T>.InSequence(Sequence: ISequence): IExpect<T>;
begin
  FCurrentSequence := Sequence;
  Result := Self;
end;

procedure TMock<T>.SetExpectedTimes(const Times: Times);
begin
  if Assigned(FCurrentSequence) then
  begin
    FCurrentSequence.ExpectInvocation(FCurrentExpectation, Times);
    FCurrentSequence := nil;
  end
  else
  begin
    FCurrentExpectation.ExpectedCount := Times;
  end;
end;

procedure TMock<T>.SetMode(const Value: TMockMode);
begin
  FMode := Value;
end;

procedure TMock<T>.Verify;
var
  LExpectation: TExpectation;
  LSequence: TList<TExpectation>;
begin
  for LExpectation in FExpectations do
  begin
    if not LExpectation.IsSatisfied then
    begin
      raise EMockException.Create(CUnmetExpectations);
    end;
  end;
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

end.
