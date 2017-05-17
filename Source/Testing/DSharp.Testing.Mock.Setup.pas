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

unit DSharp.Testing.Mock.Setup;

interface

uses
  DSharp.Testing.Mock.Interfaces,
  SysUtils;

type
  Setup<T> = record
  private
    FSetup: ISetup<T>;
  public
    class operator Implicit(const Value: ISetup<T>): Setup<T>;

    function WillExecute: IExpectInSequence<T>; overload;
    function WillExecute(const Action: TMockAction): IExpectInSequence<T>; overload;
    function WillRaise(const ExceptionClass: ExceptClass;
      const Msg: string = ''): IExpectInSequence<T>; overload;
    function WillRaise(const ExceptionClass: ExceptClass; const Msg: string;
      const Args: array of const): IExpectInSequence<T>; overload;
    function WillReturn<TResult>(const Value: TResult): IExpectInSequence<T>;
  end;

implementation

uses
  Rtti;

{ Setup<T> }

class operator Setup<T>.Implicit(const Value: ISetup<T>): Setup<T>;
begin
  Result.FSetup := Value;
end;

function Setup<T>.WillExecute: IExpectInSequence<T>;
begin
  Result := FSetup.WillExecute(nil);
end;

function Setup<T>.WillExecute(const Action: TMockAction): IExpectInSequence<T>;
begin
  Result := FSetup.WillExecute(Action);
end;

function Setup<T>.WillRaise(const ExceptionClass: ExceptClass;
  const Msg: string): IExpectInSequence<T>;
begin
  Result := FSetup.WillRaise(
    function: Exception
    begin
      Result := ExceptionClass.Create(Msg);
    end);
end;

function Setup<T>.WillRaise(const ExceptionClass: ExceptClass;
  const Msg: string; const Args: array of const): IExpectInSequence<T>;
var
  LMsg: string;
begin
  LMsg := Format(Msg, Args);
  Result := FSetup.WillRaise(
    function: Exception
    begin
      Result := ExceptClass.Create(LMsg);
    end);
end;

function Setup<T>.WillReturn<TResult>(const Value: TResult): IExpectInSequence<T>;
begin
  Result := FSetup.WillReturn(TValue.From<TResult>(Value));
end;

end.
