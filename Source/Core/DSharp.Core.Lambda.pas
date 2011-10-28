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

{$IF COMPILERVERSION < 22}{$Message Warn 'This unit is not supported in Delphi 2010'}{$IFEND}

interface

uses
  Classes,
  DSharp.Core.Expressions,
  SysUtils;

type
  Lambda = record
  public
    class function InitExpression(Expression: IExpression): IExpression; overload; static;
    class function InitExpression(const Expression: Variant): IExpression; overload; static;

    class function Make<TResult>(
      const Expression: Variant): TFunc<TResult>; overload; static;
    class function Make<T, TResult>(
      const Expression: Variant): TFunc<T, TResult>; overload; static;
    class function Make<T1, T2, TResult>(
      const Expression: Variant): TFunc<T1, T2, TResult>; overload; static;
    class function Make<T1, T2, T3, TResult>(
      const Expression: Variant): TFunc<T1, T2, T3, TResult>; overload; static;
    class function Make<T1, T2, T3, T4, TResult>(
      const Expression: Variant): TFunc<T1, T2, T3, T4, TResult>; overload; static;

    class function Predicate<T>(Expression: Variant): TPredicate<T>; overload; static;

    class function Make<TResult>(
      AExpression: IExpression): TFunc<TResult>; overload; static;
    class function Make<T, TResult>(
      AExpression: IExpression): TFunc<T, TResult>; overload; static;
    class function Make<T1, T2, TResult>(
      AExpression: IExpression): TFunc<T1, T2, TResult>; overload; static;
    class function Make<T1, T2, T3, TResult>(
      AExpression: IExpression): TFunc<T1, T2, T3, TResult>; overload; static;
    class function Make<T1, T2, T3, T4, TResult>(
      AExpression: IExpression): TFunc<T1, T2, T3, T4, TResult>; overload; static;

    class function Predicate<T>(AExpression: IExpression): TPredicate<T>; overload; static;

    class function GetExpression(var Func): IExpression; static;
    class function SetExpression(var Func;
      Expression: IExpression): Boolean; static;
  end;

function Arg(AComponent: TComponent): Variant; overload;
function Arg(AObject: TObject): Variant; overload;
function Arg(const AValue: Variant): Variant; overload;
function Arg1: Variant; overload;
function Arg1(AObject: TObject): Variant; overload;
function Arg2: Variant;
function Arg3: Variant;
function Arg4: Variant;

function Bool(const AExpression: Variant): Variant;

implementation

uses
  DSharp.Core.Variants,
  Rtti;

type
  TArgument = class(TObject)
    class threadvar FCount: Integer;
  private
    FIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
  end;

function Arg(AComponent: TComponent): Variant;
begin
  Result := AsDynamic(AComponent);
end;

function Arg(AObject: TObject): Variant;
begin
  Result := AsDynamic(AObject);
end;

function Arg(const AValue: Variant): Variant; overload;
begin
  Result := AsDynamic(AValue);
end;

function Arg1: Variant;
begin
  Result := AsDynamic(ExpressionParams[0]);
end;

function Arg1(AObject: TObject): Variant;
begin
  Result := Arg1;
  ExpressionParams[0].Value := TValue.From<TObject>(AObject);
end;

function Arg2: Variant;
begin
  Result := AsDynamic(ExpressionParams[1]);
end;

function Arg3: Variant;
begin
  Result := AsDynamic(ExpressionParams[2]);
end;

function Arg4: Variant;
begin
  Result := AsDynamic(ExpressionParams[3]);
end;

function Bool(const AExpression: Variant): Variant;
begin
  Result := AsDynamic(ExpressionStack.Peek());
end;

{ Lambda }

class function Lambda.InitExpression(Expression: IExpression): IExpression;
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

class function Lambda.InitExpression(const Expression: Variant): IExpression;
begin
  if ExpressionStack.Count = 1 then
  begin
    Result := ExpressionStack.Pop();
  end
  else
  begin
    Result := TConstantExpression.Create(Expression);
  end;
end;

class function Lambda.GetExpression(var Func): IExpression;
var
  LInterface: IInterface absolute Func;
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
      LField := LType.GetField('Expression');
      if Assigned(LField) then
      begin
        Result := LField.GetValue(LObject).AsType<IExpression>;
      end;
    end;
  end;
end;

class function Lambda.SetExpression(var Func; Expression: IExpression): Boolean;
var
  LInterface: IInterface absolute Func;
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
      LField := LType.GetField('Expression');
      if Assigned(LField) then
      begin
        LField.SetValue(LObject, TValue.From<IExpression>(Expression));
        Result := True;
      end;

      LField := LType.GetField('Delegate');
      if Assigned(LField) then
      begin
        LField.SetValue(LObject, TValue.From<TFunc<TValue>>(Expression.Compile()));
      end;
    end;
  end;
end;

class function Lambda.Make<TResult>(AExpression: IExpression): TFunc<TResult>;
var
  Expression: IExpression;
  Delegate: TFunc<TValue>;
begin
  Expression := InitExpression(AExpression);
  Delegate := Expression.Compile();

  Result :=
    function: TResult
    begin
      if Expression = nil then;
      Result := Delegate().AsType<TResult>;
    end;
end;

class function Lambda.Make<TResult>(const Expression: Variant): TFunc<TResult>;
begin
  Result := Make<TResult>(InitExpression(Expression));
end;

class function Lambda.Make<T, TResult>(AExpression: IExpression): TFunc<T, TResult>;
var
  Expression: IExpression;
  Delegate: TFunc<TValue>;
  Parameter: IValueExpression;
begin
  Expression := InitExpression(AExpression);
  Delegate := Expression.Compile();

  Parameter := ExpressionParams[0];

  Result :=
    function(Arg1: T): TResult
    begin
      if Expression = nil then;
      Parameter.Value := TValue.From<T>(Arg1);
      Result := Delegate().AsType<TResult>;
    end;
end;

class function Lambda.Make<T, TResult>(const Expression: Variant): TFunc<T, TResult>;
begin
  Result := Make<T, TResult>(InitExpression(Expression));
end;

class function Lambda.Make<T1, T2, TResult>(
  AExpression: IExpression): TFunc<T1, T2, TResult>;
var
  Expression: IExpression;
  Delegate: TFunc<TValue>;
  Parameter1, Parameter2: IValueExpression;
begin
  Expression := InitExpression(AExpression);
  Delegate := Expression.Compile();

  Parameter1 := ExpressionParams[0];
  Parameter2 := ExpressionParams[1];

  Result :=
    function(Arg1: T1; Arg2: T2): TResult
    begin
      if Expression = nil then;
      Parameter1.Value := TValue.From<T1>(Arg1);
      Parameter2.Value := TValue.From<T2>(Arg2);
      Result := Delegate().AsType<TResult>;
    end;
end;

class function Lambda.Make<T1, T2, TResult>(
  const Expression: Variant): TFunc<T1, T2, TResult>;
begin
  Result := Make<T1, T2, TResult>(InitExpression(Expression));
end;

class function Lambda.Make<T1, T2, T3, TResult>(
  AExpression: IExpression): TFunc<T1, T2, T3, TResult>;
var
  Expression: IExpression;
  Delegate: TFunc<TValue>;
  Parameter1, Parameter2, Parameter3: IValueExpression;
begin
  Expression := InitExpression(AExpression);
  Delegate := Expression.Compile();

  Parameter1 := ExpressionParams[0];
  Parameter2 := ExpressionParams[1];
  Parameter3 := ExpressionParams[2];

  Result :=
    function(Arg1: T1; Arg2: T2; Arg3: T3): TResult
    begin
      if Expression = nil then;
      Parameter1.Value := TValue.From<T1>(Arg1);
      Parameter2.Value := TValue.From<T2>(Arg2);
      Parameter3.Value := TValue.From<T3>(Arg3);
      Result := Delegate().AsType<TResult>;
    end;
end;


class function Lambda.Make<T1, T2, T3, TResult>(
  const Expression: Variant): TFunc<T1, T2, T3, TResult>;
begin
  Result := Make<T1, T2, T3, TResult>(InitExpression(Expression));
end;

class function Lambda.Make<T1, T2, T3, T4, TResult>(
  AExpression: IExpression): TFunc<T1, T2, T3, T4, TResult>;
var
  Expression: IExpression;
  Delegate: TFunc<TValue>;
  Parameter1, Parameter2, Parameter3, Parameter4: IValueExpression;
begin
  Expression := InitExpression(AExpression);
  Delegate := Expression.Compile();

  Parameter1 := ExpressionParams[0];
  Parameter2 := ExpressionParams[1];
  Parameter3 := ExpressionParams[2];
  Parameter4 := ExpressionParams[3];

  Result :=
    function(Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): TResult
    begin
      if AExpression = nil then;
      Parameter1.Value := TValue.From<T1>(Arg1);
      Parameter2.Value := TValue.From<T2>(Arg2);
      Parameter3.Value := TValue.From<T3>(Arg3);
      Parameter4.Value := TValue.From<T4>(Arg4);
      Result := Delegate().AsType<TResult>;
    end;
end;

class function Lambda.Make<T1, T2, T3, T4, TResult>(
  const Expression: Variant): TFunc<T1, T2, T3, T4, TResult>;
begin
  Result := Make<T1, T2, T3, T4, TResult>(InitExpression(Expression));
end;

class function Lambda.Predicate<T>(AExpression: IExpression): TPredicate<T>;
var
  Expression: IExpression;
  Delegate: TFunc<TValue>;
  Parameter: IValueExpression;
begin
  Expression := InitExpression(AExpression);
  Delegate := Expression.Compile();

  Parameter := ExpressionParams[0];

  Result :=
    function(Arg1: T): Boolean
    begin
      if Expression = nil then;
      Parameter.Value := TValue.From<T>(Arg1);
      Result := Delegate().AsBoolean;
    end;
end;

class function Lambda.Predicate<T>(Expression: Variant): TPredicate<T>;
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
