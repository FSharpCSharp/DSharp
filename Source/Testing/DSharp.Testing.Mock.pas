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

unit DSharp.Testing.Mock;

{$IF COMPILERVERSION < 22}
  {$MESSAGE FATAL 'This unit requires Delphi XE or higher.'}
{$IFEND}

interface

uses
  DSharp.Testing.Mock.Interfaces,
  SysUtils;

type
  Mock<T> = record
  private
    FMock: IMock<T>;
    function GetInstance: T;
    function GetMock: IMock<T>;
    property Mock: IMock<T> read GetMock;
  public
    procedure Clear;
    class operator Implicit(var Value: Mock<T>): IMock<T>;
    class operator Implicit(var Value: Mock<T>): T;
    procedure Verify;
    function WillExecute<TAction>(const Action: TAction): IExpect<T>;
    function WillRaise(const ExceptionClass: ExceptClass;
      const Msg: string = ''): IExpect<T>; overload;
    function WillRaise(const ExceptionClass: ExceptClass; const Msg: string;
      const Args: array of const): IExpect<T>; overload;
    function WillReturn<TResult>(const Value: TResult): IExpect<T>;
    property Instance: T read GetInstance;
  end;

implementation

uses
  DSharp.Testing.Mock.Internals,
  Rtti;

{ Mock<T> }

procedure Mock<T>.Clear;
begin
  FMock := nil;
end;

function Mock<T>.GetInstance: T;
begin
  Result := Mock.Instance;
end;

function Mock<T>.GetMock: IMock<T>;
begin
  if not Assigned(FMock) then
  begin
    FMock := TMockWrapper<T>.Create();
  end;
  Result := FMock;
end;

class operator Mock<T>.Implicit(var Value: Mock<T>): IMock<T>;
begin
  Result := Value.Mock;
end;

class operator Mock<T>.Implicit(var Value: Mock<T>): T;
begin
  Result := Value.Mock.Instance;
end;

procedure Mock<T>.Verify;
begin
  Mock.Verify;
end;

function Mock<T>.WillExecute<TAction>(const Action: TAction): IExpect<T>;
begin
  Result := Mock.WillExecute(TValue.From<TAction>(Action));
end;

function Mock<T>.WillRaise(const ExceptionClass: ExceptClass;
  const Msg: string): IExpect<T>;
begin
  Result := Mock.WillRaise(
    function: Exception
    begin
      Result := ExceptionClass.Create(Msg);
    end);
end;

function Mock<T>.WillRaise(const ExceptionClass: ExceptClass;
  const Msg: string; const Args: array of const): IExpect<T>;
var
  LMsg: string;
begin
  LMsg := Format(Msg, Args);
  Result := Mock.WillRaise(
    function: Exception
    begin
      Result := ExceptClass.Create(LMsg);
    end);
end;

function Mock<T>.WillReturn<TResult>(const Value: TResult): IExpect<T>;
begin
  Result := Mock.WillReturn(TValue.From<TResult>(Value));
end;

end.