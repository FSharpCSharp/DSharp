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

unit DSharp.Core.Lambda;

interface

uses
  SysUtils,
  DSharp.Core.Expressions;

type
  TLambda = record
  public
    class function InitExpression(Expression: IExpression): IExpression; overload; static; inline;
    class function InitExpression(Expression: Variant): IExpression; overload; static; inline;

    class function Make<TResult>(
      Expression: Variant): TFunc<TResult>; overload; static;
    class function Make<T, TResult>(
      Expression: Variant): TFunc<T, TResult>; overload; static;
    class function Make<T1, T2, TResult>(
      Expression: Variant): TFunc<T1, T2, TResult>; overload; static;
    class function Make<T1, T2, T3, TResult>(
      Expression: Variant): TFunc<T1, T2, T3, TResult>; overload; static;
    class function Make<T1, T2, T3, T4, TResult>(
      Expression: Variant): TFunc<T1, T2, T3, T4, TResult>; overload; static;

    class function Predicate<T>(Expression: Variant): TPredicate<T>; overload; static;

    class function Make<TResult>(
      Expression: IExpression): TFunc<TResult>; overload; static;
    class function Make<T, TResult>(
      Expression: IExpression): TFunc<T, TResult>; overload; static;
    class function Make<T1, T2, TResult>(
      Expression: IExpression): TFunc<T1, T2, TResult>; overload; static;
    class function Make<T1, T2, T3, TResult>(
      Expression: IExpression): TFunc<T1, T2, T3, TResult>; overload; static;
    class function Make<T1, T2, T3, T4, TResult>(
      Expression: IExpression): TFunc<T1, T2, T3, T4, TResult>; overload; static;

    class function Predicate<T>(Expression: IExpression): TPredicate<T>; overload; static;

    class function GetExpression(var AFunc): IExpression; static;
    class function SetExpression(var AFunc;
      AExpression: IExpression): Boolean; static;
  end;

function Arg(AObject: TObject): Variant; overload;
function Arg(AValue: Variant): Variant; overload;
function Arg1: Variant; overload;
function Arg1(AObject: TObject): Variant; overload;
function Arg2: Variant;
function Arg3: Variant;
function Arg4: Variant;

function Bool(Expression: Variant): Variant;

implementation

uses
  Rtti,
  DSharp.Core.Variants;

type
  TArgument = class(TObject)
  private
    FIndex: Integer;
    class threadvar FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
  end;

function Arg(AObject: TObject): Variant;
begin
  Result := AsDynamic(AObject);
  ExpressionStack.Push(TExpressionVarData(Result).VExpressionInfo.Expression);
end;

function Arg(AValue: Variant): Variant; overload;
begin
  Result := AsDynamic(AValue);
  ExpressionStack.Push(TExpressionVarData(Result).VExpressionInfo.Expression);
end;

function Arg1: Variant;
begin
  Result := AsDynamic(ParameterList[0]);
  ExpressionStack.Push(ParameterList[0]);
end;

function Arg1(AObject: TObject): Variant;
begin
  Result := Arg1;
  ParameterList[0].Value := TValue.From<TObject>(AObject);
end;

function Arg2: Variant;
begin
  Result := AsDynamic(ParameterList[1]);
  ExpressionStack.Push(ParameterList[1]);
end;

function Arg3: Variant;
begin
  Result := AsDynamic(ParameterList[2]);
  ExpressionStack.Push(ParameterList[2]);
end;

function Arg4: Variant;
begin
  Result := AsDynamic(ParameterList[3]);
  ExpressionStack.Push(ParameterList[3]);
end;

function Bool(Expression: Variant): Variant;
begin
  Result := AsDynamic(ExpressionStack.Peek());
end;

{ TLambda }

class function TLambda.InitExpression(Expression: IExpression): IExpression;
begin
  if ExpressionStack.Count = 1 then
  begin
    Result := ExpressionStack.Pop();
  end
  else
  begin
    Result := Expression;
  end;
end;

class function TLambda.InitExpression(Expression: Variant): IExpression;
begin
  if ExpressionStack.Count = 1 then
  begin
    Result := ExpressionStack.Pop();
  end
  else
  begin
    Result := TValueConstantExpression.Create(TValue.FromVariant(Expression));
  end;
end;

class function TLambda.GetExpression(var AFunc): IExpression;
var
  LInterface: IInterface absolute AFunc;
  LField: TRttiField;
  LObject: TObject;
  LType: TRttiType;
begin
  Result := nil;
  LObject := LInterface as TObject;
  if Assigned(LObject) then
  begin
    LType := TRttiContext.Create.GetType(LObject.ClassType);
    if Assigned(LType) then
    begin
      LField := LType.GetField('expr');
      if Assigned(LField) then
      begin
        Result := LField.GetValue(LObject).AsType<IExpression>;
      end;
    end;
  end;
end;

class function TLambda.SetExpression(var AFunc; AExpression: IExpression): Boolean;
var
  LInterface: IInterface absolute AFunc;
  LField: TRttiField;
  LObject: TObject;
  LType: TRttiType;
begin
  Result := False;
  LObject := LInterface as TObject;
  if Assigned(LObject) then
  begin
    LType := TRttiContext.Create.GetType(LObject.ClassType);
    if Assigned(LType) then
    begin
      LField := LType.GetField('expr');
      if Assigned(LField) then
      begin
        LField.SetValue(LObject, TValue.From<IExpression>(AExpression));
        Result := True;
      end;
    end;
  end;
end;

class function TLambda.Make<TResult>(Expression: IExpression): TFunc<TResult>;
var
  expr: IExpression;
begin
  expr := InitExpression(Expression);

  Result :=
    function: TResult
    begin
      Result := expr.Compile.AsType<TResult>;
    end;
end;

class function TLambda.Make<TResult>(Expression: Variant): TFunc<TResult>;
begin
  Result := Make<TResult>(InitExpression(Expression));
end;

class function TLambda.Make<T, TResult>(Expression: IExpression): TFunc<T, TResult>;
var
  expr: IExpression;
  param: IParameter;
begin
  expr := InitExpression(Expression);

  param := ParameterList[0];

  Result :=
    function(Arg1: T): TResult
    begin
      param.Value := TValue.From<T>(Arg1);
      Result := expr.Compile.AsType<TResult>;
    end;
end;

class function TLambda.Make<T, TResult>(Expression: Variant): TFunc<T, TResult>;
begin
  Result := Make<T, TResult>(InitExpression(Expression));
end;

class function TLambda.Make<T1, T2, TResult>(
  Expression: IExpression): TFunc<T1, T2, TResult>;
var
  expr: IExpression;
  param1, param2: IParameter;
begin
  expr := InitExpression(Expression);

  param1 := ParameterList[0];
  param2 := ParameterList[1];

  Result :=
    function(Arg1: T1; Arg2: T2): TResult
    begin
      param1.Value := TValue.From<T1>(Arg1);
      param2.Value := TValue.From<T2>(Arg2);
      Result := expr.Compile.AsType<TResult>;
    end;
end;

class function TLambda.Make<T1, T2, TResult>(
  Expression: Variant): TFunc<T1, T2, TResult>;
begin
  Result := Make<T1, T2, TResult>(InitExpression(Expression));
end;

class function TLambda.Make<T1, T2, T3, TResult>(
  Expression: IExpression): TFunc<T1, T2, T3, TResult>;
var
  expr: IExpression;
  param1, param2, param3: IParameter;
begin
  expr := InitExpression(Expression);

  param1 := ParameterList[0];
  param2 := ParameterList[1];
  param3 := ParameterList[2];

  Result :=
    function(Arg1: T1; Arg2: T2; Arg3: T3): TResult
    begin
      param1.Value := TValue.From<T1>(Arg1);
      param2.Value := TValue.From<T2>(Arg2);
      param3.Value := TValue.From<T3>(Arg3);
      Result := expr.Compile.AsType<TResult>;
    end;
end;


class function TLambda.Make<T1, T2, T3, TResult>(
  Expression: Variant): TFunc<T1, T2, T3, TResult>;
begin
  Result := Make<T1, T2, T3, TResult>(InitExpression(Expression));
end;

class function TLambda.Make<T1, T2, T3, T4, TResult>(
  Expression: IExpression): TFunc<T1, T2, T3, T4, TResult>;
var
  expr: IExpression;
  param1, param2, param3, param4: IParameter;
begin
  expr := InitExpression(Expression);

  param1 := ParameterList[0];
  param2 := ParameterList[1];
  param3 := ParameterList[2];
  param4 := ParameterList[3];

  Result :=
    function(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): TResult
    begin
      param1.Value := TValue.From<T1>(Arg1);
      param2.Value := TValue.From<T2>(Arg2);
      param3.Value := TValue.From<T3>(Arg3);
      param4.Value := TValue.From<T4>(Arg4);
      Result := expr.Compile.AsType<TResult>;
    end;
end;

class function TLambda.Make<T1, T2, T3, T4, TResult>(
  Expression: Variant): TFunc<T1, T2, T3, T4, TResult>;
begin
  Result := Make<T1, T2, T3, T4, TResult>(InitExpression(Expression));
end;

class function TLambda.Predicate<T>(Expression: IExpression): TPredicate<T>;
var
  expr: IExpression;
  param: IParameter;
begin
  expr := InitExpression(Expression);

  param := ParameterList[0];

  Result :=
    function(Arg1: T): Boolean
    begin
      param.Value := TValue.From<T>(Arg1);
      Result := expr.Compile.AsBoolean;
    end;
end;

class function TLambda.Predicate<T>(Expression: Variant): TPredicate<T>;
begin
  Result := Predicate<T>(InitExpression(Expression));
end;

{ TArgument }

constructor TArgument.Create;
begin
  Inc(FCount);
  FIndex := FCount;
end;

destructor TArgument.Destroy;
begin
  Dec(FCount);
  inherited;
end;

function TArgument.ToString: string;
begin
  Result := Format('Arg%d', [FIndex]);
end;

end.
