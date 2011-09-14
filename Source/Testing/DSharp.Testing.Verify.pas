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

unit DSharp.Testing.Verify;

interface

uses
  DSharp.Core.Expressions,
  TestFramework;

type
  ShouldBe = record
  public
    class function AtLeast<T>(const Value: T): TBooleanExpression; static;
    class function AtMost<T>(const Value: T): TBooleanExpression; static;
    class function Between<T>(const LowValue, HighValue: T): TBooleanExpression; static;
    class function EqualTo<T>(const Value: T): TBooleanExpression; static;
    class function GreaterThan<T>(const Value: T): TBooleanExpression; static;
    class function LessThan<T>(const Value: T): TBooleanExpression; static;
  end;

  Verify = record
  private
    class var FActual: IParameter;
  public
    class constructor Create;
    class procedure That<T>(const Value: T; Expression: TBooleanExpression;
      const Msg: string = ''); static;
    class property Actual: IParameter read FActual;
  end;

  EExpectationFailure = ETestFailure;

const
  CExpectationFailed = 'Expected: "%0:s" Actual: %1:s';

implementation

uses
  DSharp.Core.Reflection,
  Rtti;

{ ShouldBe }

class function ShouldBe.AtLeast<T>(const Value: T): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(TGreaterThanOrEqualExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(Value)));
end;

class function ShouldBe.AtMost<T>(const Value: T): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(TLessThanOrEqualExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(Value)));
end;

class function ShouldBe.Between<T>(const LowValue, HighValue: T): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(TGreaterThanOrEqualExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(LowValue))) and
    TBooleanExpression.Create(TLessThanOrEqualExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(HighValue)));
end;

class function ShouldBe.EqualTo<T>(const Value: T): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(TEqualExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(Value)));
end;

class function ShouldBe.GreaterThan<T>(const Value: T): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(TGreaterThanExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(Value)));
end;

class function ShouldBe.LessThan<T>(const Value: T): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(TLessThanExpression.Create(
    Verify.Actual, TValueConstantExpression.From<T>(Value)));
end;

{ Verify }

class constructor Verify.Create;
begin
  FActual := TParameterExpression.Create('?');
end;

class procedure Verify.That<T>(const Value: T; Expression: TBooleanExpression;
  const Msg: string);
begin
  Actual.Value := TValue.From<T>(Value);
  if not Expression.Compile.AsBoolean then
  begin
    if Msg <> '' then
    begin
      raise EExpectationFailure.CreateFmt(Msg + ' ' +  CExpectationFailed, [
        Expression.ToString, Actual.Value.ToString]);
    end
    else
    begin
      raise EExpectationFailure.CreateFmt(CExpectationFailed, [
        Expression.ToString, Actual.Value.ToString]);
    end;
  end;
end;

end.
