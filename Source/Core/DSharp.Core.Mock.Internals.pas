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

unit DSharp.Core.Mock.Internals;

interface

uses
  DSharp.Core.Mock,
  DSharp.Core.Mock.Interfaces,
  Generics.Collections,
  Rtti,
  SysUtils;

type
  TBehaviour = class
  private
    FAction: TValue;
    FArguments: TArray<TValue>;
    FCallCount: Integer;
    FMethod: TRttiMethod;
    FResult: TValue;
  public
    function Execute(const Arguments: TArray<TValue>;
      ReturnType: TRttiType): TValue;

    property Action: TValue read FAction write FAction;
    property Arguments: TArray<TValue> read FArguments write FArguments;
    property CallCount: Integer read FCallCount write FCallCount;
    property Method: TRttiMethod read FMethod write FMethod;
    property Result: TValue read FResult write FResult;
  end;

  TMockState = (msDefining, msExecuting);
  TMockType = (mtUndefined, mtObject, mtInterface);

  TMockWrapper<T> = class(TInterfacedObject, IMock<T>, IWhenCalling<T>)
  private
    FBehaviours: TObjectList<TBehaviour>;
    FCurrentBehaviour: TBehaviour;
    FInstance: T;
    FInterceptor: TVirtualMethodInterceptor;
    FMode: TMockMode;
    FState: TMockState;
    FType: TMockType;
{$IF COMPILERVERSION > 22}
    procedure CreateInterfaceMock(AType: TRttiType);
{$IFEND}
    procedure CreateObjectMock(AType: TRttiType);
    procedure DestroyObjectMock;
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoMethodCall(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue);
    function FindBehaviour(Method: TRttiMethod; Arguments: TArray<TValue>): TBehaviour;
    function GetInstance: T;
  public
    constructor Create(AMode: TMockMode);
    destructor Destroy; override;
    procedure Verify;
    function WhenCalling: T;
    function WillExecute(const Value: TValue): IWhenCalling<T>;
    function WillReturn(const Value: TValue): IWhenCalling<T>;
  end;

  EMockException = class(Exception);

const
  CCreationError = 'Unable to create mock for type: %0:s';
  CExpectationsNotMet = 'Expectation not met: %0:s(%1:s)';
  CUnexpectedCall = 'Unexpected method call: %0:s(%1:s)';

implementation

uses
  DSharp.Core.Reflection,
  TypInfo;

{ TMockWrapper<T> }

constructor TMockWrapper<T>.Create(AMode: TMockMode);
var
  LType: TRttiType;
begin
  FBehaviours := TObjectList<TBehaviour>.Create(True);
  FMode := AMode;
  LType := GetRttiType(TypeInfo(T));
  if LType is TRttiInstanceType then
  begin
    CreateObjectMock(LType);
  end else
{$IF COMPILERVERSION > 22}
  if (LType is TRttiInterfaceType)
    and (TRttiInterfaceType(LType).GUID <> TGUID.Empty())
    and (LType.MethodCount > 0) then
  begin
    CreateInterfaceMock(LType);
  end else
{$IFEND}
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
  FBehaviours.Free();
end;

{$IF COMPILERVERSION > 22}
procedure TMockWrapper<T>.CreateInterfaceMock(AType: TRttiType);
begin
  Supports(TVirtualInterface.Create(AType.Handle, DoMethodCall),
    TRttiInterfaceType(AType).GUID, FInstance);
  FType := mtInterface;
end;
{$IFEND}

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
      FCurrentBehaviour.Method := Method;
      FCurrentBehaviour.Arguments := Args;
      FState := msExecuting;
    end;
    msExecuting:
    begin
      FCurrentBehaviour := FindBehaviour(Method, Args);
      if Assigned(FCurrentBehaviour) then
      begin
        Result := FCurrentBehaviour.Execute(Args, Method.ReturnType);
      end
      else
      begin
        raise EMockException.CreateFmt(CUnexpectedCall, [
          Method.Name, TValue.ToString(Args)]);
      end;
    end;
  end;
end;

function TMockWrapper<T>.FindBehaviour(Method: TRttiMethod;
  Arguments: TArray<TValue>): TBehaviour;
var
  LBehaviour: TBehaviour;
begin
  Result := nil;

  for LBehaviour in FBehaviours do
  begin
    // skip already called behaviours when in mock mode
    if (LBehaviour.CallCount > 0) and (FMode = mmMock) then
    begin
      Continue;
    end;

    // only get behaviour if corrent method
    if (LBehaviour.Method = Method)
      // and argument values match or in stub mode
      and (TValue.Equals(LBehaviour.Arguments, Arguments) or (FMode = mmStub)) then
    begin
      Result := LBehaviour;
      Break;
    end;

    // don't look further if in mock mode
    if FMode = mmMock then
    begin
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
  LBehaviour: TBehaviour;
begin
  for LBehaviour in FBehaviours do
  begin
    if LBehaviour.CallCount = 0 then
    begin
      raise EMockException.CreateFmt(CExpectationsNotMet, [
        LBehaviour.Method.Name, TValue.ToString(LBehaviour.Arguments)]);
    end;
  end;
end;

function TMockWrapper<T>.WhenCalling: T;
begin
  Result := FInstance;
end;

function TMockWrapper<T>.WillExecute(const Value: TValue): IWhenCalling<T>;
begin
  FCurrentBehaviour := TBehaviour.Create();
  FCurrentBehaviour.Action := Value;
  Result := Self;
  FState := msDefining;
  FBehaviours.Add(FCurrentBehaviour);
end;

function TMockWrapper<T>.WillReturn(const Value: TValue): IWhenCalling<T>;
begin
  FCurrentBehaviour := TBehaviour.Create();
  FCurrentBehaviour.Result := Value;
  Result := Self;
  FState := msDefining;
  FBehaviours.Add(FCurrentBehaviour);
end;

{ TBehaviour }

function TBehaviour.Execute(const Arguments: TArray<TValue>;
  ReturnType: TRttiType): TValue;
type
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LCode: Pointer;
  LArgs: TArray<TValue>;
  i: Integer;
begin
  if not FAction.IsEmpty then
  begin
    LType := GetRttiType(FAction.TypeInfo);
    if (LType is TRttiInterfaceType) then
    begin
      if LType.TryGetMethod('Invoke', LMethod) then
      begin
        // TODO: better checking for matching parameters
        Result := LMethod.Invoke(FAction, Arguments);
      end
      else
      begin
        // Try to do it without rtti
        FAction.ExtractRawDataNoCopy(@LCode);
        SetLength(LArgs, Length(Arguments));
        TValue.Make(@LCode, TypeInfo(IInterface), LArgs[0]);
        for i := Low(Arguments) to High(Arguments) do
        begin
          LArgs[i + 1] := Arguments[i];
        end;
        if Assigned(ReturnType) then
        begin
          Result := Invoke(PPVtable(LCode)^^[3], LArgs, ccReg, ReturnType.Handle);
        end
        else
        begin
          Result := Invoke(PPVtable(LCode)^^[3], LArgs, ccReg, nil);
        end;
      end;
    end;
  end;

  Inc(FCallCount);
  if not FResult.IsEmpty then
  begin
    Result := FResult;
  end;
end;

end.
