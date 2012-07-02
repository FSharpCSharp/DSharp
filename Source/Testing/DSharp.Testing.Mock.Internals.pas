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

unit DSharp.Testing.Mock.Internals;

interface

uses
{$IF COMPILERVERSION < 23}
  DSharp.Core.VirtualInterface,
{$IFEND}
  DSharp.Testing.Mock.Interfaces,
  Generics.Collections,
  Rtti,
  SysUtils,
  TestFramework;

type
  TCountMatcher = record
  private
    FMin: Cardinal;
    FMax: Cardinal;
  public
    constructor Create(const Min, Max: Cardinal);

    property Min: Cardinal read FMin;
    property Max: Cardinal read FMax;

    class operator Equal(const Matcher: TCountMatcher; const Value: Cardinal): Boolean;
    class operator GreaterThanOrEqual(const Matcher: TCountMatcher; const Value: Cardinal): Boolean;
  end;

  TExpectation = class
  private
    FAction: TMockAction;
    FAnyArguments: Boolean;
    FArguments: TArray<TValue>;
    FCallCount: Cardinal;
    FException: TFunc<Exception>;
    FCountMatcher: TCountMatcher;
    FMethod: TRttiMethod;
    FResult: TValue;
  public
    destructor Destroy; override;

    function CanCall: Boolean;
    function Execute(const Arguments: TArray<TValue>;
      ReturnType: TRttiType): TValue;
    function HasBeenMet: Boolean;

    property Action: TMockAction read FAction write FAction;
    property AnyArguments: Boolean read FAnyArguments write FAnyArguments;
    property Arguments: TArray<TValue> read FArguments write FArguments;
    property CallCount: Cardinal read FCallCount write FCallCount;
    property Exception: TFunc<Exception> read FException write FException;
    property Matcher: TCountMatcher read FCountMatcher write FCountMatcher;
    property Method: TRttiMethod read FMethod write FMethod;
    property Result: TValue read FResult write FResult;
  end;

  TMockState = (msDefining, msExecuting);
  TMockType = (mtUndefined, mtObject, mtInterface);

  TMockWrapper<T> = class(TInterfacedObject, IMock<T>, IExpect<T>, IWhen<T>, ISequence<T>)
  private
    FCurrentExpectation: TExpectation;
    FExpectations: TObjectList<TExpectation>;
    FInstance: T;
    FInterceptor: TVirtualMethodInterceptor;
    FMode: TMockMode;
    FSequences: TDictionary<string, TList<TExpectation>>;
    FState: TMockState;
    FType: TMockType;
    procedure CreateInterfaceMock(AType: TRttiType);
    procedure CreateObjectMock(AType: TRttiType);
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
    procedure SetMode(const Value: TMockMode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Verify;

    // IExpect<T>
    function AtLeast(const Count: Cardinal): ISequence<T>;
    function AtLeastOnce: ISequence<T>;
    function AtMost(const Count: Cardinal): ISequence<T>;
    function AtMostOnce: ISequence<T>;
    function Between(const LowValue, HighValue: Cardinal): ISequence<T>;
    function Exactly(const Count: Cardinal): ISequence<T>;
    function Never: IWhen<T>;
    function Once: ISequence<T>;

    // IWhen<T>
    function WhenCalling: T;
    function WhenCallingWithAnyArguments: T;

    // ISequence<T>
    function InSequence(const Name: string = ''): IWhen<T>;

    // IMock<T>
    function WillExecute(const Action: TMockAction): IExpect<T>;
    function WillRaise(const E: TFunc<Exception>): IExpect<T>;
    function WillReturn(const Value: TValue): IExpect<T>;
  end;

  EMockException = ETestFailure;

const
  CCreationError = 'unable to create mock for type: %0:s';
  CUnmetExpectations = 'not all expected invocations were performed';
  CUnmetSequencialExpectations = 'not all sequencial invocations were performed';
  CUnexpectedInvocation = 'unexpected invocation of: %0:s.%1:s(%2:s)';

  Undefined = Cardinal(-1);

type
  Mock<T> = class(TVirtualInterface);

implementation

uses
  DSharp.Core.Reflection,
  RTLConsts,
  TypInfo;

{ TCountMatcher }

constructor TCountMatcher.Create(const Min, Max: Cardinal);
begin
  FMin := Min;
  FMax := Max;
end;

class operator TCountMatcher.Equal(const Matcher: TCountMatcher;
  const Value: Cardinal): Boolean;
begin
  Result := ((Matcher.Min = Undefined) or (Matcher.Min <= Value))
    and ((Matcher.Max = Undefined) or (Matcher.Max >= Value));
end;

class operator TCountMatcher.GreaterThanOrEqual(const Matcher: TCountMatcher;
  const Value: Cardinal): Boolean;
begin
  Result := (Matcher.Max = Undefined) or (Matcher.Max >= Value);
end;

{ TMockWrapper<T> }

constructor TMockWrapper<T>.Create;
var
  LType: TRttiType;
begin
  FExpectations := TObjectList<TExpectation>.Create(True);
  FSequences := TObjectDictionary<string, TList<TExpectation>>.Create([doOwnsValues]);
  FState := msExecuting;
  LType := GetRttiType(TypeInfo(T));
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

destructor TMockWrapper<T>.Destroy;
begin
  if FType = mtObject then
  begin
    DestroyObjectMock();
  end;
  FSequences.Free();
  FExpectations.Free();
end;

procedure TMockWrapper<T>.CreateInterfaceMock(AType: TRttiType);
begin
  Supports(Mock<T>.Create(AType.Handle, DoMethodCall),
    TRttiInterfaceType(AType).GUID, FInstance);
  FType := mtInterface;
end;

procedure TMockWrapper<T>.CreateObjectMock(AType: TRttiType);
begin
  FInstance := AType.GetStandardConstructor().Invoke(
    AType.AsInstance.MetaclassType, []).AsType<T>();
  FInterceptor := TVirtualMethodInterceptor.Create(AType.AsInstance.MetaclassType);
  FInterceptor.Proxify(PObject(@FInstance)^);
  FInterceptor.OnBefore := DoBefore;
  FType := mtObject;
end;

procedure TMockWrapper<T>.DestroyObjectMock;
begin
  PObject(@FInstance)^.Free();
  FInterceptor.Free();
end;

procedure TMockWrapper<T>.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
begin
  if Method.Parent.AsInstance.MetaclassType <> TObject then
  begin
    DoInvoke := False;

    DoMethodCall(Method, Args, Result);
  end;
end;

procedure TMockWrapper<T>.DoMethodCall(Method: TRttiMethod;
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

        for LSequence in FSequences.Values do
        begin
          if (LSequence.Count > 0) and (LSequence[0] = FCurrentExpectation)
            and not LSequence[0].CanCall then
          begin
            LSequence.Delete(0);
          end;
        end;
      end
      else
      begin
        raise EMockException.CreateFmt(CUnexpectedInvocation, [
          StripUnitName(Method.Parent.Name), Method.Name, TValue.ToString(@Args[0])]);
      end;
    end;
  end;
end;

function TMockWrapper<T>.AtLeast(const Count: Cardinal): ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(Count, Undefined);
  Result := Self;
end;

function TMockWrapper<T>.AtLeastOnce: ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(1, Undefined);
  Result := Self;
end;

function TMockWrapper<T>.AtMost(const Count: Cardinal): ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(Undefined, Count);
  Result := Self;
end;

function TMockWrapper<T>.AtMostOnce: ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(Undefined, 1);
  Result := Self;
end;

function TMockWrapper<T>.Between(const LowValue, HighValue: Cardinal): ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(LowValue, HighValue);
  Result := Self;
end;

function TMockWrapper<T>.Exactly(const Count: Cardinal): ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(Count, Count);
  Result := Self;
end;

function TMockWrapper<T>.Never: IWhen<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(0, 0);
  Result := Self;
end;

function TMockWrapper<T>.Once: ISequence<T>;
begin
  FCurrentExpectation.Matcher := TCountMatcher.Create(1, 1);
  Result := Self;
end;

function TMockWrapper<T>.FindExpectation(Method: TRttiMethod;
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
      and LExpectation.CanCall then
    begin
      Result := LExpectation;
      Break;
    end;
  end;
end;

function TMockWrapper<T>.GetInstance: T;
begin
  Result := FInstance;
end;

function TMockWrapper<T>.GetMode: TMockMode;
begin
  Result := FMode;
end;

function TMockWrapper<T>.HasExpectation(Method: TRttiMethod): Boolean;
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

function TMockWrapper<T>.InSequence(const Name: string): IWhen<T>;
var
  LSequence: TList<TExpectation>;
begin
  if not FSequences.TryGetValue(Name, LSequence) then
  begin
    LSequence := TList<TExpectation>.Create;
    FSequences.Add(Name, LSequence);
  end;
  LSequence.Add(FCurrentExpectation);
  Result := Self;
end;

procedure TMockWrapper<T>.SetMode(const Value: TMockMode);
begin
  FMode := Value;
end;

procedure TMockWrapper<T>.Verify;
var
  LExpectation: TExpectation;
  LSequence: TList<TExpectation>;
begin
  for LSequence in FSequences.Values do
  begin
    if LSequence.Count > 0 then
    begin
      raise EMockException.Create(CUnmetSequencialExpectations);
    end;
  end;

  for LExpectation in FExpectations do
  begin
    if not LExpectation.HasBeenMet then
    begin
      raise EMockException.Create(CUnmetExpectations);
    end;
  end;
end;

function TMockWrapper<T>.WhenCalling: T;
begin
  Result := FInstance;
end;

function TMockWrapper<T>.WhenCallingWithAnyArguments: T;
begin
  FCurrentExpectation.AnyArguments := True;
  Result := FInstance;
end;

function TMockWrapper<T>.WillExecute(const Action: TMockAction): IExpect<T>;
begin
  FCurrentExpectation := TExpectation.Create();
  FCurrentExpectation.Action := Action;
  Result := Self;
  FState := msDefining;
  FExpectations.Add(FCurrentExpectation);
end;

function TMockWrapper<T>.WillRaise(const E: TFunc<Exception>): IExpect<T>;
begin
  FCurrentExpectation := TExpectation.Create();
  FCurrentExpectation.Exception := E;
  Result := Self;
  FState := msDefining;
  FExpectations.Add(FCurrentExpectation);
end;

function TMockWrapper<T>.WillReturn(const Value: TValue): IExpect<T>;
begin
  FCurrentExpectation := TExpectation.Create();
  FCurrentExpectation.Result := Value;
  Result := Self;
  FState := msDefining;
  FExpectations.Add(FCurrentExpectation);
end;

{ TExpectation }

destructor TExpectation.Destroy;
begin
  inherited;
end;

function TExpectation.CanCall: Boolean;
begin
  Result := FCountMatcher >= (FCallCount + 1);
end;

function TExpectation.Execute(const Arguments: TArray<TValue>;
  ReturnType: TRttiType): TValue;
type
  PValueArray = ^TValueArray;
  TValueArray = TArray<TValue>;
begin
  try
    if Assigned(FAction) then
    begin
      Result := FAction(PValueArray(@Arguments)^);
    end else
    if Assigned(FException) then
    begin
      raise FException;
    end;
  finally
    Inc(FCallCount);
    if not FResult.IsEmpty then
    begin
      Result := FResult;
    end;
  end;
end;

function TExpectation.HasBeenMet: Boolean;
begin
  Result := FCountMatcher = FCallCount;
end;

end.
