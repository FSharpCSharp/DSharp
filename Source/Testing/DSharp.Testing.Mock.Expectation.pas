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

unit DSharp.Testing.Mock.Expectation;

interface

uses
  DSharp.Core.Times,
  DSharp.Testing.Mock.Interfaces,
  Rtti,
  SysUtils;

type
  TExpectation = class
  private
    FAction: TMockAction;
    FActualCount: Cardinal;
    FAnyArguments: Boolean;
    FArguments: TArray<TValue>;
    FCallback: TProc;
    FException: TFunc<Exception>;
    FExpectedCount: Times;
    FMethod: TRttiMethod;
    FResult: TValue;
  public
    function AllowsInvocation: Boolean;
    function Execute(const Arguments: TArray<TValue>;
      ReturnType: TRttiType): TValue;
    function IsSatisfied: Boolean;
    function ToString: string; override;

    property Action: TMockAction read FAction write FAction;
    property ActualCount: Cardinal read FActualCount write FActualCount;
    property AnyArguments: Boolean read FAnyArguments write FAnyArguments;
    property Arguments: TArray<TValue> read FArguments write FArguments;
    property Callback: TProc read FCallback write FCallback;
    property Exception: TFunc<Exception> read FException write FException;
    property ExpectedCount: Times read FExpectedCount write FExpectedCount;
    property Method: TRttiMethod read FMethod write FMethod;
    property Result: TValue read FResult write FResult;
  end;

implementation

uses
  DSharp.Core.Reflection;

resourcestring
  CExpectation = '%s was expected to be called %s and has been called %s';

{ TExpectation }

function TExpectation.AllowsInvocation: Boolean;
begin
  Result := FExpectedCount.Max >= (FActualCount + 1);
end;

function TExpectation.Execute(const Arguments: TArray<TValue>;
  ReturnType: TRttiType): TValue;
begin
  try
    if Assigned(FCallback) then
    begin
      FCallback();
    end;
    Inc(FActualCount);
    if Assigned(FAction) then
    begin
      Result := FAction(Arguments, ReturnType);
    end else
    if Assigned(FException) then
    begin
      raise FException;
    end;
  finally
    if not FResult.IsEmpty then
    begin
      Result := FResult;
    end;
  end;
end;

function TExpectation.IsSatisfied: Boolean;
begin
  Result := FExpectedCount.Verify(FActualCount);
end;

function TExpectation.ToString: string;
begin
  Result := Format(CExpectation, [FMethod.Format(FArguments, True),
    FExpectedCount.ToString, Times.Exactly(FActualCount).ToString]);
end;

end.
