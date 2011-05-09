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

unit System.Expressions;

interface

uses
  DB,
  Generics.Collections,
  Rtti,
  SysUtils;

type
  IExpression = interface
    ['{B6748486-F9E5-40E3-92E7-9DC2C5BFB3F7}']
    function Compile: TValue;
    function ToString: string;
  end;

  IParameter = interface(IExpression)
    ['{E276DEFC-4223-4709-AF19-75F8EEF7104E}']
    function GetValue: TValue;
    procedure SetValue(const Value: TValue);
    property Value: TValue read GetValue write SetValue;
  end;

  IUnaryExpression = interface(IExpression)
    function GetValue: IExpression;
    property Value: IExpression read GetValue;
  end;

  IBinaryExpression = interface(IExpression)
    function GetLeft: IExpression;
    function GetRight: IExpression;
    property Left: IExpression read GetLeft;
    property Right: IExpression read GetRight;
  end;

  TEntityBase = class
  end;

  TBooleanExpression = record
  private
    FValue: IExpression;
  public
    constructor Create(Value: IExpression);
    class operator Equal(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator LogicalAnd(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator LogicalOr(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator LogicalXor(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator Negative(const Value: TBooleanExpression): TBooleanExpression;
    class operator NotEqual(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator Implicit(Value: Boolean): TBooleanExpression;
    class operator Implicit(Value: TBooleanExpression): Boolean;
    class operator Implicit(Value: TBooleanExpression): TFunc<Boolean>;
    class operator Implicit(Value: TBooleanExpression): TFunc<TObject, Boolean>;
    function Compile: TValue;
    function ToString: string;
  end;

  TValueExpression = record
  private
    FValue: IExpression;
  public
    class operator Add(const Left, Right: TValueExpression): TValueExpression;
    class operator Subtract(const Left, Right: TValueExpression): TValueExpression;
    class operator Multiply(const Left, Right: TValueExpression): TValueExpression;
    class operator IntDivide(const Left, Right: TValueExpression): TValueExpression;
    class operator Divide(const Left, Right: TValueExpression): TValueExpression;
    class operator Modulus(const Left, Right: TValueExpression): TValueExpression;
    class operator Negative(const Value: TValueExpression): TValueExpression;
    class operator Equal(const Left, Right: TValueExpression): TBooleanExpression;
    class operator NotEqual(const Left, Right: TValueExpression): TBooleanExpression;
    class operator GreaterThan(const Left, Right: TValueExpression): TBooleanExpression;
    class operator GreaterThanOrEqual(const Left, Right: TValueExpression): TBooleanExpression;
    class operator LessThan(const Left, Right: TValueExpression): TBooleanExpression;
    class operator LessThanOrEqual(const Left, Right: TValueExpression): TBooleanExpression;
    class operator Implicit(Value: Extended): TValueExpression;
    class operator Implicit(Value: Integer): TValueExpression;
    class operator Implicit(Value: TBooleanExpression): TValueExpression;
    class operator Implicit(Value: Variant): TValueExpression;
    class operator Implicit(Name: TField): TValueExpression;
    class operator Implicit(Name: TObject): TValueExpression;
    class operator Implicit(Value: string): TValueExpression;
    class operator Implicit(Value: TValueExpression): Extended;
    class operator Implicit(Value: TValueExpression): Integer;
    function Compile: TValue;
    function ToString: string;
  end;

  TUnaryExpression = class(TInterfacedObject, IExpression, IUnaryExpression)
  private
    FValue: IExpression;
    function GetValue: IExpression;
  public
    constructor Create(Value: IExpression);
    function Compile: TValue; virtual; abstract;
    property Value: IExpression read GetValue;
  end;

  TNegativeExpression = class(TUnaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TBinaryExpression = class(TInterfacedObject, IExpression, IBinaryExpression)
  private
    FLeft: IExpression;
    FRight: IExpression;
    function GetLeft: IExpression;
    function GetRight: IExpression;
  public
    constructor Create(Left, Right: IExpression);
    function Compile: TValue; virtual; abstract;
    property Left: IExpression read GetLeft;
    property Right: IExpression read GetRight;
  end;

  TBinaryExpressionClass = class of TBinaryExpression;

  TAdditionExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TSubtractionExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TMultiplicationExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TIntegerDivisionExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TDivisionExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TModulusExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TEqualExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TNotEqualExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TGreaterThanExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TGreaterThanOrEqualExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TLessThanExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TLessThanOrEqualExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TLogicalAndExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TLogicalOrExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TLogicalXorExpression = class(TBinaryExpression)
  public
    function Compile: TValue; override;
    function ToString: string; override;
  end;

  TConstantExpression = class(TInterfacedObject, IExpression)
  public
    function Compile: TValue; virtual; abstract;
  end;

  TBooleanConstantExpression = class(TConstantExpression)
  private
    FValue: Boolean;
  public
    constructor Create(Value: Boolean);
    function Compile: TValue; override;
    function ToString: string; override;
    property Value: Boolean read FValue;
  end;

  TFloatConstantExpression = class(TConstantExpression)
  private
    FValue: Double;
  public
    constructor Create(Value: Double);
    function Compile: TValue; override;
    function ToString: string; override;
    property Value: Double read FValue;
  end;

  TIntegerConstantExpression = class(TConstantExpression)
  private
    FValue: Integer;
  public
    constructor Create(Value: Integer);
    function Compile: TValue; override;
    function ToString: string; override;
    property Value: Integer read FValue;
  end;

  TStringConstantExpression = class(TConstantExpression)
  private
    FValue: string;
  public
    constructor Create(Value: string);
    function Compile: TValue; override;
    function ToString: string; override;
    property Value: string read FValue;
  end;

  TValueConstantExpression = class(TConstantExpression, IParameter)
  private
    FValue: TValue;
    procedure SetValue(const Value: TValue);
    function GetValue: TValue;
  public
    constructor Create(Value: TValue);
    function Compile: TValue; override;
    function ToString: string; override;
    property Value: TValue read GetValue write SetValue;
  end;

  TParameterExpression = class(TValueConstantExpression)
  public
    function ToString: string; override;
  end;

  TEntityExpression = class(TInterfacedObject, IExpression)
  private
    FName: string;
  public
    constructor Create(Name: string);
    function Compile: TValue;
    function ToString: string; override;
    property Value: string read FName;
  end;

  TPropertyExpression = class(TInterfacedObject, IExpression)
  private
    FExpression: IExpression;
    FPropertyName: string;
    function GetMethod(AObject: TObject): TRttiMethod;
    function GetObject: TObject;
    function GetProperty(AObject: TObject): TRttiProperty;
  public
    constructor Create(AExpression: IExpression; APropertyName: string);
    function Compile: TValue;
    function ToString: string; override;
  end;

  TMethodExpression = class(TInterfacedObject, IExpression)
  private
    FExpression: IExpression;
    FMethodName: string;
    FParameters: TArray<IExpression>;
    function GetMethod(AObject: TObject): TRttiMethod;
    function GetObject: TObject;
    function GetParameters: TArray<TValue>;
  public
    constructor Create(AExpression: IExpression; AMethodName: string; AParameters: TArray<IExpression>);
    function Compile: TValue;
    function ToString: string; override;
  end;

  TMaxExpression = class(TInterfacedObject, IExpression)
  private
    FParam: TValueExpression;
  public
    constructor Create(Param: TValueExpression);
    function Compile: TValue;
    function ToString: string; override;
    property Param: TValueExpression read FParam;
  end;

var
  ExpressionStack: TStack<IExpression>;
  ParameterList: array[0..3] of IParameter;

function Bool(value: Boolean): TBooleanExpression;

function Max(E: TValueExpression): TValueExpression;

implementation

uses
  TypInfo;

var
  Context: TRttiContext;

type
  TValueHelper = record helper for TValue
  public
    function IsFloat: Boolean;
    function IsNumeric: Boolean;
    function IsString: Boolean;
  end;

function Bool(value: Boolean): TBooleanExpression;
begin
  Result := TBooleanExpression.Create(ExpressionStack.Peek);
end;

{ TValueHelper }

function TValueHelper.IsFloat: Boolean;
begin
  Result := Self.Kind = tkFloat;
end;

function TValueHelper.IsNumeric: Boolean;
const
  tkNumeric = [tkInteger, {tkChar, }tkWChar, tkEnumeration, tkFloat, tkInt64];
begin
  Result := Self.Kind in tkNumeric;
end;

function TValueHelper.IsString: Boolean;
begin
  Result := Self.Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString];
end;

{ TBooleanExpression }

constructor TBooleanExpression.Create(Value: IExpression);
begin
  FValue := Value
end;

class operator TBooleanExpression.Equal(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FValue := TEqualExpression.Create(Left.FValue, Right.FValue);
end;

class operator TBooleanExpression.Implicit(
  Value: TBooleanExpression): TFunc<Boolean>;
var
  Expr: IExpression;
begin
  Expr := ExpressionStack.Pop();
//  ParameterList.Clear();

  Result :=
    function: Boolean
    begin
      Result := Expr.Compile().AsBoolean();
    end;
end;

class operator TBooleanExpression.Implicit(
  Value: TBooleanExpression): TFunc<TObject, Boolean>;
var
  Expr: IExpression;
  Param1: IParameter;
begin
  Expr := ExpressionStack.Pop();
  Param1 := ParameterList[0];  // todo: check for actual params
//  ParameterList.Clear();

  Result :=
    function(Arg1: TObject): Boolean
    begin
      Param1.SetValue(Arg1);
      Result := Expr.Compile().AsBoolean();
    end;
end;

class operator TBooleanExpression.Implicit(Value: Boolean): TBooleanExpression;
begin
  Result.FValue := TBooleanConstantExpression.Create(Value);
end;

class operator TBooleanExpression.Implicit(Value: TBooleanExpression): Boolean;
begin
  Result := Value.Compile.AsBoolean;
end;

class operator TBooleanExpression.LogicalAnd(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FValue := TLogicalAndExpression.Create(Left.FValue, Right.FValue);
  ExpressionStack.Pop();
  ExpressionStack.Pop();
  ExpressionStack.Push(Result.FValue);
end;

class operator TBooleanExpression.LogicalOr(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FValue := TLogicalOrExpression.Create(Left.FValue, Right.FValue);
  ExpressionStack.Pop();
  ExpressionStack.Pop();
  ExpressionStack.Push(Result.FValue);
end;

class operator TBooleanExpression.LogicalXor(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FValue := TLogicalXorExpression.Create(Left.FValue, Right.FValue);
  ExpressionStack.Pop();
  ExpressionStack.Pop();
  ExpressionStack.Push(Result.FValue);
end;

class operator TBooleanExpression.Negative(
  const Value: TBooleanExpression): TBooleanExpression;
begin
  Result.FValue := TNegativeExpression.Create(Value.FValue);
  ExpressionStack.Pop();
  ExpressionStack.Push(Result.FValue);
end;

class operator TBooleanExpression.NotEqual(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FValue := TNotEqualExpression.Create(Left.FValue, Right.FValue);
  ExpressionStack.Pop();
  ExpressionStack.Pop();
  ExpressionStack.Push(Result.FValue);
end;

function TBooleanExpression.Compile: TValue;
begin
  Result := FValue.Compile;
end;

function TBooleanExpression.ToString: string;
begin
  Result := FValue.ToString;
end;

{ TValueExpression }

class operator TValueExpression.Add(const Left, Right: TValueExpression): TValueExpression;
begin
  Result.FValue := TAdditionExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Divide(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FValue := TDivisionExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Equal(const Left, Right: TValueExpression): TBooleanExpression;
begin
  Result.FValue := TEqualExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.GreaterThan(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FValue := TGreaterThanExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.GreaterThanOrEqual(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FValue := TGreaterThanOrEqualExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Implicit(Name: TObject): TValueExpression;
begin
  Result.FValue := TEntityExpression.Create(Name.ToString);
end;

class operator TValueExpression.Implicit(Name: TField): TValueExpression;
begin
  Result.FValue := TEntityExpression.Create(Name.FieldName);
end;

class operator TValueExpression.Implicit(Value: Extended): TValueExpression;
begin
  Result.FValue := TFloatConstantExpression.Create(Value);
end;

class operator TValueExpression.Implicit(Value: Integer): TValueExpression;
begin
  Result.FValue := TIntegerConstantExpression.Create(Value);
end;

class operator TValueExpression.Implicit(Value: string): TValueExpression;
begin
  Result.FValue := TStringConstantExpression.Create(Value);
end;

class operator TValueExpression.Implicit(Value: TValueExpression): Extended;
begin
  Result := Value.Compile.AsExtended;
end;

class operator TValueExpression.Implicit(Value: TValueExpression): Integer;
begin
  Result := Trunc(Value.Compile.AsExtended);
end;

class operator TValueExpression.Implicit(Value: Variant): TValueExpression;
begin
  case TVarData(Value).VType of
    varByRef or varVariant:
    begin
      Result.FValue := IExpression(TVarData(Value).VPointer);
      Result.FValue._Release;
    end;
  else
    raise ENotSupportedException.Create('Variant conversion');
  end;
end;

class operator TValueExpression.Implicit(Value: TBooleanExpression): TValueExpression;
begin
  Result.FValue := Value.FValue;
end;

class operator TValueExpression.IntDivide(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FValue := TIntegerDivisionExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.LessThan(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FValue := TLessThanExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.LessThanOrEqual(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FValue := TLessThanOrEqualExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Modulus(const Left, Right: TValueExpression): TValueExpression;
begin
  Result.FValue := TModulusExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Multiply(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FValue := TMultiplicationExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Negative(const Value: TValueExpression): TValueExpression;
begin
  Result.FValue := TNegativeExpression.Create(Value.FValue);
end;

class operator TValueExpression.NotEqual(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FValue := TNotEqualExpression.Create(Left.FValue, Right.FValue);
end;

class operator TValueExpression.Subtract(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FValue := TSubtractionExpression.Create(Left.FValue, Right.FValue);
end;

function TValueExpression.Compile: TValue;
begin
  Result := FValue.Compile;
end;

function TValueExpression.ToString: string;
begin
  Result := FValue.ToString;
end;

{ TBinaryExpression }

constructor TBinaryExpression.Create(Left, Right: IExpression);
begin
  FLeft := Left;
  FRight := Right;
end;

function TBinaryExpression.GetLeft: IExpression;
begin
  Result := FLeft;
end;

function TBinaryExpression.GetRight: IExpression;
begin
  Result := FRight;
end;

{ TUnaryExpression }

constructor TUnaryExpression.Create(Value: IExpression);
begin
  FValue := Value;
end;

function TUnaryExpression.GetValue: IExpression;
begin
  Result := FValue;
end;

{ TBooleanConstantExpression }

function TBooleanConstantExpression.Compile: TValue;
begin
  Result := TValue.From<Boolean>(FValue);
end;

constructor TBooleanConstantExpression.Create(Value: Boolean);
begin
  FValue := Value;
end;

function TBooleanConstantExpression.ToString: string;
begin
  Result := BoolToStr(FValue, True);
end;

{ TFloatConstantExpression }

function TFloatConstantExpression.Compile: TValue;
begin
  Result := TValue.From<Double>(FValue);
end;

constructor TFloatConstantExpression.Create(Value: Double);
begin
  FValue := Value;
end;

function TFloatConstantExpression.ToString: string;
begin
  Result := FloatToStr(FValue);
end;

{ TIntegerConstantExpression }

function TIntegerConstantExpression.Compile: TValue;
begin
  Result := TValue.From<Integer>(FValue);
end;

constructor TIntegerConstantExpression.Create(Value: Integer);
begin
  FValue := Value;
end;

function TIntegerConstantExpression.ToString: string;
begin
  Result := IntToStr(FValue);
end;

{ TStringConstantExpression }

function TStringConstantExpression.Compile: TValue;
begin
  Result := TValue.From<string>(FValue);
end;

constructor TStringConstantExpression.Create(Value: string);
begin
  FValue := Value;
end;

function TStringConstantExpression.ToString: string;
begin
  Result := '''' + StringReplace(FValue, '''', '''''', [rfReplaceAll]) + '''';
end;

{ TAdditionExpression }

function TAdditionExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsOrdinal and LRight.IsOrdinal then
  begin
    Result := TValue.From<Int64>(LLeft.AsOrdinal + LRight.AsOrdinal);
  end;
  if LLeft.IsFloat and LRight.IsFloat then
  begin
    Result := TValue.From<Extended>(LLeft.AsExtended + LRight.AsExtended);
  end;
  if LLeft.IsString and LRight.IsString then
  begin
    Result := TValue.From<string>(LLeft.AsString + LRight.AsString);
  end;
end;

function TAdditionExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' + ' + FRight.ToString + ')';
end;

{ TSubtractionExpression }

function TSubtractionExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsOrdinal and LRight.IsOrdinal then
  begin
    Result := TValue.From<Int64>(LLeft.AsOrdinal - LRight.AsOrdinal);
  end;
  if LLeft.IsFloat and LRight.IsFloat then
  begin
    Result := TValue.From<Extended>(LLeft.AsExtended - LRight.AsExtended);
  end;
end;

function TSubtractionExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' - ' + FRight.ToString + ')';
end;

{ TMultiplicationExpression }

function TMultiplicationExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsOrdinal and LRight.IsOrdinal then
  begin
    Result := TValue.From<Int64>(LLeft.AsOrdinal * LRight.AsOrdinal);
  end;
  if LLeft.IsFloat and LRight.IsFloat then
  begin
    Result := TValue.From<Extended>(LLeft.AsExtended * LRight.AsExtended);
  end;
end;

function TMultiplicationExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' * ' + FRight.ToString + ')';
end;

{ TDivisionExpression }

function TDivisionExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Extended>(LLeft.AsExtended / LRight.AsExtended);
  end;
end;

function TDivisionExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' / ' + FRight.ToString + ')';
end;

{ TIntegerDivisionExpression }

function TIntegerDivisionExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsOrdinal and LRight.IsOrdinal then
  begin
    Result := TValue.From<Int64>(LLeft.AsOrdinal div LRight.AsOrdinal);
  end;
end;

function TIntegerDivisionExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' div ' + FRight.ToString + ')';
end;

{ TModulusExpression }

function TModulusExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsOrdinal and LRight.IsOrdinal then
  begin
    Result := TValue.From<Int64>(LLeft.AsOrdinal mod LRight.AsOrdinal);
  end;
end;

function TModulusExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' mod ' + FRight.ToString + ')';
end;

{ TEqualExpression }

function TEqualExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Boolean>(LLeft.AsExtended = LRight.AsExtended);
  end
  else
  begin
    Result := TValue.From<Boolean>(LLeft.ToString = LRight.ToString);
  end;
end;

function TEqualExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' = ' + FRight.ToString + ')';
end;

{ TNotEqualExpression }

function TNotEqualExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Boolean>(LLeft.AsExtended <> LRight.AsExtended);
  end
  else
  begin
    Result := TValue.From<Boolean>(LLeft.ToString <> LRight.ToString);
  end;
end;

function TNotEqualExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' <> ' + FRight.ToString + ')';
end;

{ TGreaterThanExpression }

function TGreaterThanExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Boolean>(LLeft.AsExtended > LRight.AsExtended);
  end
  else
  begin
    Result := TValue.From<Boolean>(LLeft.ToString > LRight.ToString);
  end;
end;

function TGreaterThanExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' > ' + FRight.ToString + ')';
end;

{ TGreaterEqualThanExpression }

function TGreaterThanOrEqualExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Boolean>(LLeft.AsExtended >= LRight.AsExtended);
  end
  else
  begin
    Result := TValue.From<Boolean>(LLeft.ToString >= LRight.ToString);
  end;
end;

function TGreaterThanOrEqualExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' >= ' + FRight.ToString + ')';
end;

{ TLessThanExpression }

function TLessThanExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Boolean>(LLeft.AsExtended < LRight.AsExtended);
  end
  else
  begin
    Result := TValue.From<Boolean>(LLeft.ToString < LRight.ToString);
  end;
end;

function TLessThanExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' < ' + FRight.ToString + ')';
end;

{ TLessEqualThanExpression }

function TLessThanOrEqualExpression.Compile: TValue;
var
  LLeft: TValue;
  LRight: TValue;
begin
  LLeft := FLeft.Compile;
  LRight := FRight.Compile;
  if LLeft.IsNumeric and LRight.IsNumeric then
  begin
    Result := TValue.From<Boolean>(LLeft.AsExtended <= LRight.AsExtended);
  end
  else
  begin
    Result := TValue.From<Boolean>(LLeft.ToString <= LRight.ToString);
  end;
end;

function TLessThanOrEqualExpression.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' <= ' + FRight.ToString + ')';
end;
{ TNegativeExpression }

function TNegativeExpression.Compile: TValue;
var
  LValue: TValue;
begin
  LValue := FValue.Compile;
  if LValue.IsOrdinal  then
  begin
    Result := TValue.From<Int64>(LValue.AsOrdinal * -1);
  end;
  if LValue.IsFloat then
  begin
    Result := TValue.From<Extended>(LValue.AsExtended * -1);
  end;
end;

function TNegativeExpression.ToString: string;
begin
  Result := '(-' + FValue.ToString + ')';
end;

{ TEntityExpression }

function TEntityExpression.Compile: TValue;
begin
  Result := TValue.From<string>(FName);
//  raise ENotImplemented.Create('TEntityExpression.Compile');
end;

constructor TEntityExpression.Create(Name: string);
begin
  FName := Name;
end;

function TEntityExpression.ToString: string;
begin
  Result := '[' + FName + ']';
end;

{ TMaxExpression }

constructor TMaxExpression.Create(Param: TValueExpression);
begin
  FParam := Param;
end;

function TMaxExpression.Compile: TValue;
begin
  raise ENotImplemented.Create('TMaxExpression.Compile');
end;

function TMaxExpression.ToString: string;
begin
  Result := 'Max(' + FParam.ToString + ')';
end;

function Max(E: TValueExpression): TValueExpression;
begin
  Result.FValue := TMaxExpression.Create(E);
end;

{ TLogicalAndExpression }

function TLogicalAndExpression.Compile: TValue;
begin
  Result := TValue.From<Boolean>(FLeft.Compile.AsBoolean and FRight.Compile.AsBoolean);
end;

function TLogicalAndExpression.ToString: string;
begin
  Result := '('  + FLeft.ToString + ' and ' + FRight.ToString + ')';
end;

{ TLogicalOrExpression }

function TLogicalOrExpression.Compile: TValue;
begin
  Result := TValue.From<Boolean>(FLeft.Compile.AsBoolean or FRight.Compile.AsBoolean);
end;

function TLogicalOrExpression.ToString: string;
begin
  Result := '('  + FLeft.ToString + ' or ' + FRight.ToString + ')';
end;

{ TLogicalXorExpression }

function TLogicalXorExpression.Compile: TValue;
begin
  Result := TValue.From<Boolean>(FLeft.Compile.AsBoolean xor FRight.Compile.AsBoolean);
end;

function TLogicalXorExpression.ToString: string;
begin
  Result := '('  + FLeft.ToString + ' xor ' + FRight.ToString + ')';
end;

{ TPropertyExpression }

function TPropertyExpression.Compile: TValue;
var
  LMethod: TRttiMethod;
  LObject: TObject;
  LProperty: TRttiProperty;
begin
  Result := TValue.Empty;
  LObject := GetObject();
  LProperty := GetProperty(LObject);
  if Assigned(LProperty) then
  begin
    Result := LProperty.GetValue(LObject);
  end
  else
  begin
    LMethod := GetMethod(LObject);
    if Assigned(LMethod) then
    begin
      if LMethod.IsClassMethod then
      begin
        Result := LMethod.Invoke(LObject.ClassType, []);
      end
      else
      begin
        Result := LMethod.Invoke(LObject, []);
      end;
    end;
  end;
end;

constructor TPropertyExpression.Create(AExpression: IExpression; APropertyName: string);
begin
  FExpression := AExpression;
  FPropertyName := APropertyName;
end;

function TPropertyExpression.GetMethod(AObject: TObject): TRttiMethod;
var
  LType: TRttiType;
begin
  if Assigned(AObject) then
  begin
    LType := Context.GetType(AObject.ClassType);
    Result := LType.GetMethod(FPropertyName);
  end
  else
  begin
    Result := nil;
  end;
end;

function TPropertyExpression.GetObject: TObject;
begin
  if Assigned(FExpression) then
  begin
    Result := FExpression.Compile.AsObject;
  end
  else
  begin
    Result := nil;
  end;
end;

function TPropertyExpression.GetProperty(AObject: TObject): TRttiProperty;
var
  LType: TRttiType;
begin
  if Assigned(AObject) then
  begin
    LType := Context.GetType(AObject.ClassType);
    Result := LType.GetProperty(FPropertyName);
  end
  else
  begin
    Result := nil;
  end;
end;

function TPropertyExpression.ToString: string;
begin
  Result := FExpression.ToString() + '.' + FPropertyName;
end;

{ TMethodExpression }

function TMethodExpression.Compile: TValue;
var
  LMethod: TRttiMethod;
  LObject: TObject;
  LParameters: TArray<TValue>;
begin
  Result := TValue.Empty;
  LObject := GetObject();
  LMethod := GetMethod(LObject);
  if Assigned(LMethod) then
  begin
    LParameters := GetParameters();
    if LMethod.IsClassMethod then
    begin
      Result := LMethod.Invoke(LObject.ClassType, LParameters);
    end
    else
    begin
      Result := LMethod.Invoke(LObject, LParameters);
    end;
  end
end;

constructor TMethodExpression.Create(AExpression: IExpression; AMethodName: string;
  AParameters: TArray<IExpression>);
begin
  FExpression := AExpression;
  FMethodName := AMethodName;
  FParameters := AParameters;
end;

function TMethodExpression.GetMethod(AObject: TObject): TRttiMethod;
var
  LType: TRttiType;
begin
  if Assigned(AObject) then
  begin
    LType := Context.GetType(AObject.ClassType);
    Result := LType.GetMethod(FMethodName);
  end
  else
  begin
    Result := nil;
  end;
end;

function TMethodExpression.GetObject: TObject;
begin
  if Assigned(FExpression) then
  begin
    Result := FExpression.Compile.AsObject;
  end
  else
  begin
    Result := nil;
  end;
end;

function TMethodExpression.GetParameters: TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(FParameters));
  for i := Low(FParameters) to High(FParameters) do
  begin
    Result[i] := FParameters[i].Compile();
  end;
end;

function TMethodExpression.ToString: string;
var
  i: Integer;
begin
  Result := FExpression.ToString() + '.' + FMethodName + '(';
  for i := Low(FParameters) to High(FParameters) do
  begin
    if i < High(FParameters) then
    begin
      Result := Result + FParameters[i].ToString() + ', ';
    end
    else
    begin
      Result := Result + FParameters[i].ToString() + ')';
    end;
  end;
end;

{ TValueConstantExpression }

function TValueConstantExpression.Compile: TValue;
begin
  Result := FValue;
end;

constructor TValueConstantExpression.Create(Value: TValue);
begin
  FValue := Value;
end;

function TValueConstantExpression.GetValue: TValue;
begin
  Result := FValue;
end;

procedure TValueConstantExpression.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

function TValueConstantExpression.ToString: string;
begin
  Result := FValue.ToString;
end;

{ TParameterExpression }

function TParameterExpression.ToString: string;
begin
  if FValue.IsObject then
  begin
    Result := FValue.AsObject.ToString;
  end
  else
  begin
    Result := FValue.ToString;
  end;
end;

initialization
  Context := TRttiContext.Create();
  ExpressionStack := TStack<IExpression>.Create();

finalization
  ExpressionStack.Free();

end.
