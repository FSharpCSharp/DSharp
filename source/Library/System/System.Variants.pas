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

unit System.Variants;

interface

function ToVar(AObject: TObject): Variant; overload;
function ToVar(AObject: Variant): Variant; overload;

implementation

uses
  Rtti,
  System.Expressions,
  Variants;

type
  TExpressionInfo = class
    Expression: IExpression;
    Instance: TObject;
  end;

  TExpressionVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VExpressionInfo: TExpressionInfo;
    Reserved4: LongWord;
  end;

  TBoxedObjectVariantType = class(TInvokeableVariantType)
  private
    class var Context: TRttiContext;
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
  BoxedObject: TBoxedObjectVariantType;

function ToVar(AObject: TObject): Variant;
begin
  VarClear(Result);

  with TExpressionVarData(Result) do
  begin
    VType := BoxedObject.VarType;
    VExpressionInfo := TExpressionInfo.Create;
    VExpressionInfo.Instance := AObject;
  end;
end;

function ToVar(AObject: Variant): Variant;
begin
  VarClear(Result);

  with TExpressionVarData(Result) do
  begin
    VType := BoxedObject.VarType;
    VExpressionInfo := TExpressionInfo.Create;
    VExpressionInfo.Instance := TExpressionVarData(AObject).VExpressionInfo.Instance;
  end;
end;


function VarDataIsBoolean(const V: TVarData): Boolean;
begin
  Result := V.VType = varBoolean;
end;

{ TBoxedObjectVariantType }

procedure TBoxedObjectVariantType.BinaryOp(var Left: TVarData;
  const Right: TVarData; const &Operator: TVarOp);
begin
  inherited;

end;

procedure TBoxedObjectVariantType.Cast(var Dest: TVarData;
  const Source: TVarData);
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
            TBooleanConstantExpression.Create(LTemp.VBoolean);
        end
        else
        begin
          if VarDataIsOrdinal(LSource) then
          begin
            VarDataCastTo(LTemp, LSource, varInteger);
            TExpressionVarData(Dest).VExpressionInfo := TExpressionInfo.Create;
            TExpressionVarData(Dest).VExpressionInfo.Expression :=
              TIntegerConstantExpression.Create(LTemp.VInteger);
          end
          else
          begin
            if VarDataIsFloat(LSource) then
            begin
              VarDataCastTo(LTemp, LSource, varDouble);
              TExpressionVarData(Dest).VExpressionInfo := TExpressionInfo.Create;
              TExpressionVarData(Dest).VExpressionInfo.Expression :=
                TFloatConstantExpression.Create(LTemp.VDouble);
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

procedure TBoxedObjectVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  if Assigned(TExpressionVarData(V).VExpressionInfo) then
    TExpressionVarData(V).VExpressionInfo.Free;
end;

function TBoxedObjectVariantType.CompareOp(const Left, Right: TVarData;
  const &Operator: TVarOp): Boolean;
var
  LExpression: IExpression;
begin
  Result := False;
  if (Left.VType = VarType) and (Right.VType = VarType) then
    case Operator of
      opCmpEQ:
      begin
        LExpression := TEqualExpression.Create(
          TExpressionVarData(Left).VExpressionInfo.Expression,
          TExpressionVarData(Right).VExpressionInfo.Expression);
        ExpressionStack.Push(LExpression);
        Result := LExpression.Compile.AsBoolean;
      end;
      opCmpNE:
      begin
        LExpression := TNotEqualExpression.Create(
          TExpressionVarData(Left).VExpressionInfo.Expression,
          TExpressionVarData(Right).VExpressionInfo.Expression);
        Result := LExpression.Compile.AsBoolean;
      end
    else
      RaiseInvalidOp;
    end
  else
    RaiseInvalidOp;
end;

procedure TBoxedObjectVariantType.Copy(var Dest: TVarData;
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

function TBoxedObjectVariantType.DoFunction(var Dest: TVarData;
  const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  LMethod: TRttiMethod;
  LParameters: array of IExpression;
  i: Integer;
begin
  with TExpressionVarData(V) do
  begin
    LMethod := Context.GetType(VExpressionInfo.Instance.ClassInfo).GetMethod(Name);
    Result := Assigned(LMethod);

    if Result then
    begin
      for i := 0 to Pred(Length(Arguments)) do
      begin
        SetLength(LParameters, Succ(i));
        if Arguments[i].VType = VarType then
        begin
          LParameters[i] := TExpressionVarData(Arguments[i]).VExpressionInfo.Expression;
        end
        else
        begin
          LParameters[i] := nil;
        end;
      end;

      Dest.VType := VarType;
      VExpressionInfo.Expression := TMethodExpression.Create(VExpressionInfo.Instance, LMethod, LParameters);
      Copy(Dest, V, False);
    end
    else
    begin
      Clear(Dest);
    end;
  end;
end;

function TBoxedObjectVariantType.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  LProperty: TRttiProperty;
begin
  with TExpressionVarData(V) do
  begin
    LProperty := Context.GetType(VExpressionInfo.Instance.ClassInfo).GetProperty(Name);
    Result := Assigned(LProperty);

    if Result then
    begin
      Dest.VType := VarType;
      VExpressionInfo.Expression := TPropertyExpression.Create(VExpressionInfo.Instance, LProperty);
      Copy(Dest, V, False);
    end;
  end;
end;

initialization
  BoxedObject := TBoxedObjectVariantType.Create();

finalization
  BoxedObject.Free();;

end.
