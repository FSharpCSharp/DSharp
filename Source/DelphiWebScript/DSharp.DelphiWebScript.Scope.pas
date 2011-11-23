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

unit DSharp.DelphiWebScript.Scope;

interface

uses
  dwsCompiler,
  dwsLanguageExtension,
  dwsSymbols,
  Rtti;

type
  TScope = class(TdwsLanguageExtension)
  private
    FType: TRttiType;
    FValue: TValue;
    procedure SetValue(const Value: TValue);
  public
    function FindUnknownName(compiler: TdwsCompiler; const name: string): TSymbol; override;
    property Value: TValue read FValue write SetValue;
  end;

function ValueToVariant(const Value: TValue): Variant;

implementation

uses
  dwsExprs,
  dwsRTTIConnector,
  dwsUtils,
  TypInfo;

var
  Context: TRttiContext;

type
  TScopeMember = class(TInterfacedSelfObject, IExecutable, ICallable)
  private
    FMember: TRttiMember;
    FScope: TScope;
  public
    constructor Create(Scope: TScope; Member: TRttiMember);

    procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
    procedure InitSymbol(symbol: TSymbol);
    procedure InitExpression(expr: TExprBase);
  end;

function ValueToVariant(const Value: TValue): Variant;
begin
  case Value.Kind of
    tkInteger, tkEnumeration, tkInt64:
      if Value.IsType<Boolean> then
        Result := Value.AsBoolean
      else
        Result:= Value.AsOrdinal;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      Result := Value.AsString;
    tkFloat: Result := Value.AsExtended;
    tkClass: Result := IUnknown(TdwsRTTIVariant.From(Value.AsObject, Context.GetType(Value.TypeInfo)));
    tkVariant: Result := Value.AsVariant;
    tkInterface: Result := IUnknown(TdwsRTTIVariant.From(Pointer(Value.AsInterface), Context.GetType(Value.TypeInfo)));
  else
    Result := Value.AsVariant;
  end;
end;

{ TScope }

function TScope.FindUnknownName(compiler: TdwsCompiler; const name: string): TSymbol;
var
  i: Integer;
  LFuncSymbol: TFuncSymbol;
  LMember: TRttiMember;
  LParams: TArray<TRttiParameter>;
  LScopeSymbol: TRTTIConnectorSymbol;
  LTable: TSymbolTable;
  LVarSymbol: TExternalVarSymbol;
begin
  Result := nil;
  if Assigned(FType) then
  begin
    LScopeSymbol := TRTTIConnectorSymbol(compiler.CurrentProg.Table.FindSymbol(SYS_RTTIVARIANT, cvMagic, TRTTIConnectorSymbol));
    if Assigned(LScopeSymbol) then
    begin
      LTable := compiler.CurrentProg.Root.Table;

      LMember := FType.GetProperty(name);
      if not Assigned(LMember) then
      begin
        LMember := FType.GetField(name);
      end;
      if not Assigned(LMember) then
      begin
        LMember := FType.GetMethod(name);
      end;
      if Assigned(LMember) then
      begin
        LFuncSymbol := TFuncSymbol.Create(name, fkFunction, 0);
        LFuncSymbol.Typ := LScopeSymbol;
        LFuncSymbol.Executable := TScopeMember.Create(Self, LMember);

        if (LMember is TRttiProperty) or (LMember is TRttiField) then
        begin
          LVarSymbol := TExternalVarSymbol.Create(name, LScopeSymbol);
          LVarSymbol.Typ := LScopeSymbol;
          LVarSymbol.ReadFunc := LFuncSymbol;
          LFuncSymbol := TFuncSymbol.Create(name, fkProcedure, 0);
          LFuncSymbol.Executable := TScopeMember.Create(Self, LMember);
          LFuncSymbol.AddParam(TParamSymbol.Create('Value', LScopeSymbol));
          LVarSymbol.WriteFunc := LFuncSymbol;
          LTable.AddSymbol(LVarSymbol);
          Result := LVarSymbol;
        end else
        if LMember is TRttiMethod then
        begin
          LParams := TRttiMethod(LMember).GetParameters;
          for i := Low(LParams) to High(LParams) do
          begin
            LFuncSymbol.AddParam(TParamSymbol.Create(LParams[i].Name, LScopeSymbol));
          end;
          LTable.AddSymbol(LFuncSymbol);
          Result := LFuncSymbol;
        end;
      end;
    end;
  end;
end;

procedure TScope.SetValue(const Value: TValue);
begin
  FValue := Value;
  if FValue.IsEmpty then
  begin
    FType := nil;
  end
  else
  begin
    FType := Context.GetType(FValue.TypeInfo);
  end;
end;

{ TScopeMember }

constructor TScopeMember.Create(Scope: TScope; Member: TRttiMember);
begin
  FMember := Member;
  FScope := Scope;
end;

procedure TScopeMember.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
  i: Integer;
  LInfo: TProgramInfo;
  LParams: TArray<TRttiParameter>;
  LParamsData: TArray<TValue>;
  LPointer: Pointer;
  LResult: Variant;
begin
  LInfo := exec.AcquireProgramInfo(func);
  try
    if FScope.Value.Kind = tkInterface then
    begin
      LPointer := Pointer(FScope.Value.AsInterface);
    end
    else
    begin
      LPointer := FScope.Value.AsObject;
    end;

    if FMember is TRttiProperty then
    begin
      if func.Kind = fkFunction then
      begin
        LResult := ValueToVariant(TRttiProperty(FMember).GetValue(LPointer));
      end
      else
      begin
        TRttiProperty(FMember).SetValue(LPointer, TValue.FromVariant(LInfo.ParamAsVariant[0]));
      end;
    end
    else if FMember is TRttiField then
    begin
      if func.Kind = fkFunction then
      begin
        LResult := ValueToVariant(TRttiField(FMember).GetValue(LPointer));
      end
      else
      begin
        TRttiField(FMember).SetValue(LPointer, TValue.FromVariant(LInfo.ParamAsVariant[0]));
      end;
    end
    else if FMember is TRttiMethod then
    begin
      LParams := TRttiMethod(FMember).GetParameters;

      SetLength(LParamsData, func.Params.Count);
      for i := 0 to High(LParamsData) do
      begin
        LParamsData[i] := TValue.FromVariant(LInfo.ParamAsVariant[i]);
      end;

      if TRttiMethod(FMember).IsClassMethod then
        LResult := ValueToVariant(TRttiMethod(FMember).Invoke(TObject(LPointer).ClassType, LParamsData))
      else
        LResult := ValueToVariant(TRttiMethod(FMember).Invoke(TObject(LPointer), LParamsData));
    end;
    if func.Kind = fkFunction then
    begin
      LInfo.ResultAsVariant := LResult;
    end;
  finally
    exec.ReleaseProgramInfo(LInfo);
  end;
end;

procedure TScopeMember.InitExpression(expr: TExprBase);
begin
  // nothing to do
end;

procedure TScopeMember.InitSymbol(symbol: TSymbol);
begin
  // nothing to do
end;

end.
