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

unit DSharp.Testing.DUnit;

interface

uses
  Rtti,
  SysUtils,
  TestFramework;

type
  TTestingAttribute = class(TCustomAttribute)
  protected
    FValues: TArray<TValue>;
    procedure InitValues(const Values: string);
  public
    property Values: TArray<TValue> read FValues;
  end;

  TestCaseAttribute = class(TTestingAttribute)
  public
    constructor Create(const Values: string);
  end;

  ValuesAttribute = class(TTestingAttribute)
  public
    constructor Create(const Values: string);
  end;

  RangeAttribute = class(TTestingAttribute)
  public
    constructor Create(const LowValue, HighValue: Extended; const Step: Extended = 1);
  end;

  ExpectedExceptionAttribute = class(TCustomAttribute)
  private
    fExceptionType: ExceptionClass;
    fNoExceptionMessage: string;
  public
    constructor Create(exceptionType: ExceptionClass;
      const noExceptionMessage: string = ''); 
    property ExceptionType: ExceptionClass read fExceptionType;
    property NoExceptionMessage: string read fNoExceptionMessage;
  end;

  TTestCase = class(TestFramework.TTestCase)
  private
    FMethod: TRttiMethod;
    FArgs: TArray<TValue>;
    FExpect: ExpectedExceptionAttribute;
  protected
    procedure Invoke(AMethod: TTestMethod); override;
  public
    constructor Create(Method: TRttiMethod; const Args: TArray<TValue>); reintroduce;
    function GetName: string; override;
    class function Suite: ITestSuite; override;
    class procedure RegisterTest(const SuitePath: string = '');
  end;

  TTestSuite = class(TestFramework.TTestSuite)
  public
    procedure AddTests(testClass: TTestCaseClass); override;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DUnitConsts,
  StrUtils,
  Types,
  TypInfo;

type
  TTestCaseClassInherited = class of TTestCase;

{ TTestingAttribute }

procedure TTestingAttribute.InitValues(const Values: string);
var
  LValues: TStringDynArray;
  i: Integer;
begin
  LValues := SplitString(Values, ';');
  SetLength(FValues, Length(LValues));
  for i := 0 to Pred(Length(LValues)) do
  begin
    FValues[i] := LValues[i];
  end;
end;

{ TestCaseAttribute }

constructor TestCaseAttribute.Create(const Values: string);
begin
  InitValues(Values);
end;

{ ValuesAttribute }

constructor ValuesAttribute.Create(const Values: string);
begin
  InitValues(Values);
end;

{ RangeAttribute }

constructor RangeAttribute.Create(const LowValue, HighValue, Step: Extended);
var
  i: Integer;
begin
  SetLength(FValues, Trunc((HighValue - LowValue) / Step + 1));
  for i := 0 to Pred(Length(FValues)) do
  begin
    FValues[i] := LowValue + i * Step;
  end;
end;

{ TTestCase }

constructor TTestCase.Create(Method: TRttiMethod; const Args: TArray<TValue>);
var
  i: Integer;
begin
  inherited Create(Method.Name);

  FMethod := Method;
  SetLength(FArgs, Length(Args));
  for i := Low(Args) to High(Args) do
  begin
    FArgs[i] := Args[i];
  end;
end;

function TTestCase.GetName: string;
begin
  if Assigned(FMethod) then
  begin
    Result := FMethod.Name + '(' + TValue.ToString(FArgs) + ')';
  end
  else
  begin
    Result := inherited;
  end;
end;

procedure TTestCase.Invoke(AMethod: TTestMethod);
begin
  if Assigned(FExpect) then
    StartExpectingException(FExpect.ExceptionType);
  if Assigned(FMethod) then
  begin
    FMethod.Invoke(Self, FArgs);
  end
  else
  begin
    inherited;
  end;
  if Assigned(FExpect) then
    StopExpectingException(FExpect.NoExceptionMessage);
end;

class procedure TTestCase.RegisterTest(const SuitePath: string = '');
begin
  TestFramework.RegisterTest(SuitePath, Suite);
end;

class function TTestCase.Suite: ITestSuite;
begin
  Result := TTestSuite.Create(Self);
end;

{ ExpectedExceptionAttribute }

constructor ExpectedExceptionAttribute.Create(exceptionType: ExceptionClass;
  const noExceptionMessage: string);
begin
  inherited Create;

  if not Assigned(exceptionType) then
    raise EArgumentNilException.Create('exceptionType');

  fExceptionType := exceptionType;
  fNoExceptionMessage := noExceptionMessage;
end;

{ TTestSuite }

procedure TTestSuite.AddTests(testClass: TTestCaseClass);
var
  i: Integer;
  LArgs: TArray<TValue>;
  LAttribute: TestCaseAttribute;
  LMethod: TRttiMethod;
  LParameters: TArray<TRttiParameter>;
  LTestSuite: ITestSuite;
  LTestCase: TTestCase;

  procedure InternalInvoke(Index: Integer);
  var
    LAttribute: TTestingAttribute;
    LValue: TValue;
  begin
    for LAttribute in LParameters[Index].GetCustomAttributes<TTestingAttribute> do
    begin
      for LValue in LAttribute.Values do
      begin
        LValue.TryConvert(LParameters[Index].ParamType.Handle, LArgs[Index]);

        if Index = Pred(Length(LParameters)) then
        begin
          LTestSuite.AddTest(TTestCaseClassInherited(testClass).Create(LMethod, LArgs));
        end
        else
        begin
          InternalInvoke(Index + 1);
        end;
      end;
    end;
  end;

begin
  for LMethod in GetRttiType(testClass).GetMethods do
  begin
    if LMethod.Visibility = mvPublished then
    begin
      LParameters := LMethod.GetParameters;
      SetLength(LArgs, Length(LParameters));

      if Length(LParameters) > 0 then
      begin
        LTestSuite := TTestSuite.Create(LMethod.Name);
        AddTest(LTestSuite);

        for LAttribute in LMethod.GetCustomAttributes<TestCaseAttribute> do
        begin
          for i := 0 to Pred(Length(LParameters)) do
          begin
            LAttribute.Values[i].TryConvert(
              LParameters[i].ParamType.Handle, LArgs[i]);
          end;

          LTestCase := TTestCaseClassInherited(testClass).Create(LMethod, LArgs);
          LTestCase.FExpect := LMethod.GetCustomAttribute<ExpectedExceptionAttribute>;
          LTestSuite.AddTest(LTestCase);
        end;

        InternalInvoke(0);
      end
      else
      begin
        LTestCase := TTestCase(testClass.Create(LMethod.Name));
        LTestCase.FExpect := LMethod.GetCustomAttribute<ExpectedExceptionAttribute>;
        AddTest(LTestCase);
      end;
    end;
  end;
end;

end.
