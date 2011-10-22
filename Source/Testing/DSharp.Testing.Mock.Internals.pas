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
  DSharp.Testing.Mock.Interfaces,
  Generics.Collections,
  Rtti,
  SysUtils,
  TestFramework;

type
  TExpectation = class
  private
    FAction: TValue;
    FAnyArguments: Boolean;
    FArguments: TArray<TValue>;
    FCallCount: Cardinal;
    FException: TFunc<Exception>;
    FMethod: TRttiMethod;
    FRequiredCountMatcher: TPredicate<Cardinal>;
    FResult: TValue;
  public
    destructor Destroy; override;

    function Execute(const Arguments: TArray<TValue>;
      ReturnType: TRttiType): TValue;
    function HasBeenMet: Boolean;

    property Action: TValue read FAction write FAction;
    property AnyArguments: Boolean read FAnyArguments write FAnyArguments;
    property Arguments: TArray<TValue> read FArguments write FArguments;
    property CallCount: Cardinal read FCallCount write FCallCount;
    property Exception: TFunc<Exception> read FException write FException;
    property RequiredCountMatcher: TPredicate<Cardinal>
      read FRequiredCountMatcher write FRequiredCountMatcher;
    property Method: TRttiMethod read FMethod write FMethod;
    property Result: TValue read FResult write FResult;
  end;

  TMockState = (msDefining, msExecuting);
  TMockType = (mtUndefined, mtObject, mtInterface);

  TMockWrapper<T> = class(TInterfacedObject, IMock<T>, IExpect<T>, IWhen<T>)
  private
    FCurrentExpectation: TExpectation;
    FExpectations: TObjectList<TExpectation>;
    FInstance: T;
    FInterceptor: TVirtualMethodInterceptor;
    FState: TMockState;
    FType: TMockType;
    procedure CreateInterfaceMock(AType: TRttiType);
    procedure CreateObjectMock(AType: TRttiType);
    procedure DestroyObjectMock;
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoMethodCall(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue);
    function FindExpectation(Method: TRttiMethod; Arguments: TArray<TValue>): TExpectation;
    function GetInstance: T;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Verify;

    // IExpect<T>
    function AtLeast(const Count: Cardinal): IWhen<T>;
    function AtLeastOnce: IWhen<T>;
    function AtMost(const Count: Cardinal): IWhen<T>;
    function Between(const LowValue, HighValue: Cardinal): IWhen<T>;
    function Exactly(const Count: Cardinal): IWhen<T>;
    function Never: IWhen<T>;
    function Once: IWhen<T>;

    // IWhen<T>
    function WhenCalling: T;
    function WhenCallingWithAnyArguments: T;

    // IMock<T>
    function WillExecute(const Action: TValue): IExpect<T>;
    function WillRaise(const E: TFunc<Exception>): IExpect<T>;
    function WillReturn(const Value: TValue): IExpect<T>;
  end;

  EMockException = ETestFailure;

const
  CCreationError = 'unable to create mock for type: %0:s';
  CUnmetExpectations = 'not all expected invocations were performed';
  CUnexpectedInvocation = 'unexpected invocation of: %0:s.%1:s(%2:s)';

implementation

uses
  DSharp.Core.Reflection,
{$IF COMPILERVERSION < 23}
  DSharp.Core.VirtualInterface,
{$IFEND}
  RTLConsts,
  TypInfo;

{ TMockWrapper<T> }

constructor TMockWrapper<T>.Create;
var
  LType: TRttiType;
begin
  FExpectations := TObjectList<TExpectation>.Create(True);
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
  FExpectations.Free();
end;

procedure TMockWrapper<T>.CreateInterfaceMock(AType: TRttiType);
begin
  Supports(TVirtualInterface.Create(AType.Handle, DoMethodCall),
    TRttiInterfaceType(AType).GUID, FInstance);
  FType := mtInterface;
end;

procedure TMockWrapper<T>.CreateObjectMock(AType: TRttiType);
begin
  FInstance := AType.GetMethod('Create').Invoke(
    AType.AsInstance.MetaclassType, []).AsType<T>();
  FInterceptor := TVirtualMethodInterceptor.Create(AType.AsInstance.MetaclassType);
  FInterceptor.Proxify(TObject(FInstance));
  FInterceptor.OnBefore := DoBefore;
  FType := mtObject;
end;

procedure TMockWrapper<T>.DestroyObjectMock;
begin
  TObject(FInstance).Free();
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
      FCurrentExpectation := FindExpectation(Method, Args);
      if Assigned(FCurrentExpectation) then
      begin
        Result := FCurrentExpectation.Execute(Args, Method.ReturnType);
      end
      else
      begin
        raise EMockException.CreateFmt(CUnexpectedInvocation, [
          Method.Parent.Name, Method.Name, TValue.ToString(@Args[0])]);
      end;
    end;
  end;
end;

function TMockWrapper<T>.AtLeast(const Count: Cardinal): IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := CallCount >= Count;
    end;
  Result := Self;
end;

function TMockWrapper<T>.AtLeastOnce: IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := CallCount >= 1;
    end;
  Result := Self;
end;

function TMockWrapper<T>.AtMost(const Count: Cardinal): IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := CallCount <= Count;
    end;
  Result := Self;
end;

function TMockWrapper<T>.Between(const LowValue, HighValue: Cardinal): IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := (CallCount >= LowValue) and (CallCount <= HighValue);
    end;
  Result := Self;
end;

function TMockWrapper<T>.Exactly(const Count: Cardinal): IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := CallCount = Count;
    end;
  Result := Self;
end;

function TMockWrapper<T>.Never: IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := CallCount = 0;
    end;
  Result := Self;
end;

function TMockWrapper<T>.Once: IWhen<T>;
begin
  FCurrentExpectation.RequiredCountMatcher :=
    function(CallCount: Cardinal): Boolean
    begin
      Result := CallCount = 1;
    end;
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
    // skip expectations not matching the required count
    if not LExpectation.RequiredCountMatcher(LExpectation.CallCount + 1) then
    begin
      Continue;
    end;

    // only get expectation if corrent method
    if (LExpectation.Method = Method)
      // and argument values match if they need to
      and (LExpectation.AnyArguments  or TValue.Equals(LExpectation.Arguments, Arguments)) then
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

procedure TMockWrapper<T>.Verify;
var
  LExpectation: TExpectation;
begin
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

function TMockWrapper<T>.WillExecute(const Action: TValue): IExpect<T>;
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

function TExpectation.Execute(const Arguments: TArray<TValue>;
  ReturnType: TRttiType): TValue;
type
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..3] of Pointer;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
  LCode: Pointer;
  LArgs: TArray<TValue>;
  i: Integer;
begin
  try
    if not FAction.IsEmpty then
    begin
      LType := GetRttiType(FAction.TypeInfo);
      if (LType is TRttiInterfaceType) and LType.TryGetMethod('Invoke', LMethod) then
      begin
        LParameters := LMethod.GetParameters();

        if Pred(Length(Arguments)) <> Length(LParameters) then
          raise EInvocationError.CreateRes(@SParameterCountMismatch);

        SetLength(LArgs, Length(Arguments) - 1);
        for i := Low(Arguments) + 1 to High(Arguments) do
        begin
          TValue.MakeWithoutCopy(Arguments[i].GetReferenceToRawData,
            LParameters[i - 1].ParamType.Handle, LArgs[i - 1]);
        end;
        LMethod.Invoke(FAction, LArgs);
        for i := Low(Arguments) + 1 to High(Arguments) do
        begin
          TValue.MakeWithoutCopy(LArgs[i - 1].GetReferenceToRawData,
            LParameters[i - 1].ParamType.Handle, Arguments[i]);
        end;
      end
      else
      begin
        // Try to do it without rtti
        // We handle every parameter as by val - for any other parameter
        // this causes an AV - provide RTTI for the method in that case

        FAction.ExtractRawDataNoCopy(@LCode);
        LCode := PPVtable(LCode)^^[3];
        SetLength(LArgs, Length(Arguments));
        for i := 0 to Length(Arguments) - 1 do
        begin
          TValue.MakeWithoutCopy(Arguments[i].GetReferenceToRawData,
            Arguments[i].TypeInfo, LArgs[i]);
        end;
        if Assigned(ReturnType) then
        begin
          Result := Invoke(LCode, LArgs, ccReg, ReturnType.Handle);
        end
        else
        begin
          Result := Invoke(LCode, LArgs, ccReg, nil);
        end;
      end;
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
  Result := FRequiredCountMatcher(FCallCount);
end;

end.
