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

unit DSharp.Core.Variants;

interface

uses
  DSharp.Core.Expressions;

function AsDynamic(AExpression: IExpression): Variant; overload;
function AsDynamic(AObject: TObject): Variant; overload;
function AsDynamic(AValue: Variant): Variant; overload;

type
  TExpressionInfo = class
    Expression: IExpression;
    Instance: TObject;
  end;

  TExpressionVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VExpressionInfo: TExpressionInfo;
    Reserved4: NativeUInt;
  end;

implementation

uses
  Rtti,
  Variants;

type
  TExpressionVariantType = class(TInvokeableVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData;
      const Operator: TVarOp); override;
    function CompareOp(const Left, Right: TVarData;
      const Operator: TVarOp): Boolean; override;
  end;

var
  Expression: TExpressionVariantType;

  OperatorExpressions: array[opAdd..opCmpGE] of TBinaryExpressionClass = (
    TAdditionExpression, TSubtractionExpression, TMultiplicationExpression,
    TDivisionExpression, TIntegerDivisionExpression, TModulusExpression,
    nil, nil, TLogicalAndExpression, TLogicalOrExpression, TLogicalXorExpression,
    nil, nil, nil, TEqualExpression, TNotEqualExpression,
    TLessThanExpression, TLessThanOrEqualExpression, TGreaterThanExpression,
    TGreaterThanOrEqualExpression);

function AsDynamic(AExpression: IExpression): Variant;
begin
  VarClear(Result);

  with TExpressionVarData(Result) do
  begin
    VType := Expression.VarType;
    VExpressionInfo := TExpressionInfo.Create;
    VExpressionInfo.Expression := AExpression;
  end;
end;

function AsDynamic(AObject: TObject): Variant;
begin
  VarClear(Result);

  with TExpressionVarData(Result) do
  begin
    VType := Expression.VarType;
    VExpressionInfo := TExpressionInfo.Create;
    VExpressionInfo.Expression := TValueConstantExpression.Create(TValue.From<TObject>(AObject));
    VExpressionInfo.Instance := AObject;
  end;
end;

function AsDynamic(AValue: Variant): Variant; overload;
begin
  VarClear(Result);

  with TExpressionVarData(Result) do
  begin
    VType := Expression.VarType;
    VExpressionInfo := TExpressionInfo.Create;
    VExpressionInfo.Expression := TValueConstantExpression.Create(TValue.FromVariant(AValue));
  end;
end;

{ TExpressionVariantType }

procedure TExpressionVariantType.BinaryOp(var Left: TVarData;
  const Right: TVarData; const &Operator: TVarOp);
var
  LLeft, LRight: IExpression;
begin
  if (Right.VType = VarType) and (Left.VType = VarType) then
  begin
    LRight := ExpressionStack.Pop();
    LLeft := ExpressionStack.Pop();
    TExpressionVarData(Left).VExpressionInfo.Expression :=
      OperatorExpressions[Operator].Create(LLeft, LRight);
    ExpressionStack.Push(TExpressionVarData(Left).VExpressionInfo.Expression);
  end
  else
  begin
    inherited;
  end;
end;

procedure TExpressionVariantType.Cast(var Dest: TVarData;
  const Source: TVarData);

  function VarDataIsBoolean(const V: TVarData): Boolean;
  begin
    Result := V.VType = varBoolean;
  end;

var
  LSource, LTemp: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataCopyNoInd(LSource, Source);
    VarDataClear(Dest);
    if VarDataIsStr(LSource) then
    begin
      TExpressionVarData(Dest).VExpressionInfo := TExpressionInfo.Create;
      TExpressionVarData(Dest).VExpressionInfo.Expression :=
        TStringConstantExpression.Create(VarDataToStr(LSource));
      ExpressionStack.Push(TExpressionVarData(Dest).VExpressionInfo.Expression);
    end
    else
    begin
      VarDataInit(LTemp);
      try
        if VarDataIsBoolean(LSource) then
        begin
          VarDataCastTo(LTemp, LSource, varBoolean);
          TExpressionVarData(Dest).VExpressionInfo := TExpressionInfo.Create;
          TExpressionVarData(Dest).VExpressionInfo.Expression :=
            TValueConstantExpression.Create(TValue.FromVariant(Variant(LTemp)));
          ExpressionStack.Push(TExpressionVarData(Dest).VExpressionInfo.Expression);
        end
        else
        begin
          if VarDataIsOrdinal(LSource) then
          begin
            VarDataCastTo(LTemp, LSource, varInteger);
            TExpressionVarData(Dest).VExpressionInfo := TExpressionInfo.Create;
            TExpressionVarData(Dest).VExpressionInfo.Expression :=
              TIntegerConstantExpression.Create(LTemp.VInteger);
            ExpressionStack.Push(TExpressionVarData(Dest).VExpressionInfo.Expression);
          end
          else
          begin
            if VarDataIsFloat(LSource) then
            begin
              VarDataCastTo(LTemp, LSource, varDouble);
              TExpressionVarData(Dest).VExpressionInfo := TExpressionInfo.Create;
              TExpressionVarData(Dest).VExpressionInfo.Expression :=
                TFloatConstantExpression.Create(LTemp.VDouble);
              ExpressionStack.Push(TExpressionVarData(Dest).VExpressionInfo.Expression);
            end;
          end;
        end;
      finally
        VarDataClear(LTemp);
      end;
    end;
    Dest.VType := VarType;
  finally
    VarDataClear(LSource);
  end;
end;

procedure TExpressionVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  if Assigned(TExpressionVarData(V).VExpressionInfo) then
    TExpressionVarData(V).VExpressionInfo.Free;
end;

function TExpressionVariantType.CompareOp(const Left, Right: TVarData;
  const &Operator: TVarOp): Boolean;
var
  LLeft, LRight: IExpression;
begin
  if (Left.VType = VarType) and (Right.VType = VarType) then
  begin
    LRight := ExpressionStack.Pop();
    LLeft := ExpressionStack.Pop();
    ExpressionStack.Push(OperatorExpressions[Operator].Create(LLeft, LRight));
    Result := False; // does not matter
  end
  else
  begin
    Result := inherited;
  end;
end;

procedure TExpressionVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    with TExpressionVarData(Dest) do
    begin
      VType := VarType;
      VExpressionInfo := TExpressionInfo.Create();
      VExpressionInfo.Expression := TExpressionVarData(Source).VExpressionInfo.Expression;
      VExpressionInfo.Instance := TExpressionVarData(Source).VExpressionInfo.Instance;
    end;
  end;
end;

function TExpressionVariantType.DoFunction(var Dest: TVarData;
  const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  i: Integer;
  LParameter: IExpression;
  LParameters: TArray<IExpression>;
begin
  with TExpressionVarData(V) do
  begin
    LParameter := ExpressionStack.Pop();
    SetLength(LParameters, Length(Arguments));
    for i := Low(Arguments) to High(Arguments) do
    begin
      if Arguments[i].VType = VarType then
      begin
        LParameters[i] := TExpressionVarData(Arguments[i]).VExpressionInfo.Expression;
      end
      else
      begin
        LParameters[i] := TValueConstantExpression.Create(TValue.FromVariant(Variant(Arguments[i])));
      end;
    end;

    Result := True;
    Dest.VType := VarType;
    VExpressionInfo.Expression := TMethodExpression.Create(LParameter, Name, LParameters);
    ExpressionStack.Push(VExpressionInfo.Expression);
    Copy(Dest, V, False);
  end;
end;

function TExpressionVariantType.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  LParameter: IExpression;
begin
  with TExpressionVarData(V) do
  begin
    LParameter := ExpressionStack.Pop();

    Result := True;
    Dest.VType := VarType;
    VExpressionInfo.Expression := TPropertyExpression.Create(LParameter, Name);
    ExpressionStack.Push(VExpressionInfo.Expression);
    Copy(Dest, V, False);
  end;
end;

initialization
  Expression := TExpressionVariantType.Create();

finalization
  Expression.Free();;

end.
