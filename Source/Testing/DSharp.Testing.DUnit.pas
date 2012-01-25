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

unit DSharp.Testing.DUnit;

interface

uses
  Rtti,
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

  TTestCase = class(TestFramework.TTestCase)
  protected
    FMethod: TRttiMethod;
    FArgs: TArray<TValue>;
    procedure Invoke(AMethod: TTestMethod); override;
  public
    constructor Create(Method: TRttiMethod; const Args: TArray<TValue>); reintroduce;
    function GetName: string; override;
    class function Suite: ITestSuite; override;
  end;

  TTestSuite = class(TestFramework.TTestSuite)
  public
    procedure AddTests(testClass: TTestCaseClass); override;
  end;

implementation

uses
  DSharp.Core.Reflection,
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
  Result := FMethod.Name + '(' + TValue.ToString(FArgs) + ')';
end;

procedure TTestCase.Invoke(AMethod: TTestMethod);
begin
  FMethod.Invoke(Self, FArgs);
end;

class function TTestCase.Suite: ITestSuite;
begin
  Result := TTestSuite.Create(Self);
end;

{ TTestSuite }

procedure TTestSuite.AddTests(testClass: TTestCaseClass);
var
  LMethod: TRttiMethod;
  LArgs: TArray<TValue>;
  LAttribute: TCustomAttribute;
  LParameters: TArray<TRttiParameter>;
  i: Integer;
  LTestSuite: ITestSuite;

  procedure InternalInvoke(Index: Integer);
  var
    LAttribute: TCustomAttribute;
    LValue: TValue;
  begin
    for LAttribute in LParameters[Index].GetAttributes do
    begin
      if LAttribute is TTestingAttribute then
      begin
        for LValue in TTestingAttribute(LAttribute).Values do
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
  end;

begin
  for LMethod in GetRttiType(testClass).GetMethods do
  begin
    if LMethod.Visibility = mvPublished then
    begin
      LTestSuite := TTestSuite.Create(LMethod.Name);
      AddTest(LTestSuite);

      LParameters := LMethod.GetParameters;
      SetLength(LArgs, Length(LParameters));

      for LAttribute in LMethod.GetAttributes do
      begin
        if LAttribute is TestCaseAttribute then
        begin
          for i := 0 to Pred(Length(LParameters)) do
          begin
            TestCaseAttribute(LAttribute).Values[i].TryConvert(
              LParameters[i].ParamType.Handle, LArgs[i]);
          end;

          LTestSuite.AddTest(TTestCaseClassInherited(testClass).Create(LMethod, LArgs));
        end;
      end;

      InternalInvoke(0);
    end;
  end;
end;

end.
