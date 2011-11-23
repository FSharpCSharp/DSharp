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

unit DSharp.DelphiWebScript.Expression;

interface

uses
  Classes,
  DSharp.Core.Expressions,
  DSharp.DelphiWebScript.Scope,
  Rtti,
  SysUtils;

type
  TDelphiWebScriptExpression = class(TMemberExpression, IValueExpression)
  private
    FExpression: IExpression;
    FScope: TScope;
    FText: string;
  protected
    procedure SetValue(const Value: TValue); override;
  public
    constructor Create(AExpression: IExpression; const AText: string); overload;
    constructor Create(AObject: TObject; const AText: string); overload;
    destructor Destroy; override;

    function Compile: TFunc<TValue>; override;
    function ToString: string; override;
  end;

function ScriptExpression(const ExpressionText: string; Scope: TObject = nil): IExpression;

implementation

uses
  DSharp.DelphiWebScript.Connector,
  dwsExprs,
  dwsRTTIConnector,
  StrUtils,
  TypInfo;

function ScriptExpression(const ExpressionText: string; Scope: TObject = nil): IExpression;
begin
  Result := TDelphiWebScriptExpression.Create(Scope, ExpressionText);
end;

{ TDelphiWebScriptExpression }

constructor TDelphiWebScriptExpression.Create(AExpression: IExpression;
  const AText: string);
begin
  FExpression := AExpression;
  FScope := TScope.Create;
  FText := AText;
end;

constructor TDelphiWebScriptExpression.Create(AObject: TObject; const AText: string);
var
  LExpression: IExpression;
begin
  LExpression := TConstantExpression.Create(AObject);
  Create(LExpression, AText);
end;

destructor TDelphiWebScriptExpression.Destroy;
begin
  FScope.Free;
end;

procedure TDelphiWebScriptExpression.SetValue(const Value: TValue);
var
  LProgram: IdwsProgram;
  LExecution: IdwsProgramExecution;
  LVariant: Variant;
begin
  if FText <> EmptyStr then
  begin
    FScope.Value := FExpression.Execute();
    LProgram := TDelphiWebScriptConnector.Compile('var Value: RttiVariant; ' + FText + ' := Value', FScope);
    if not LProgram.Msgs.HasErrors then
    begin
      LExecution := LProgram.BeginNewExecution;
      try
        LVariant := ValueToVariant(Value);
        LExecution.Info.Vars['Value'].Value := LVariant;
        LExecution.RunProgram(0);
      finally
        LExecution.EndProgram;
      end;
    end;
  end;
end;

function TDelphiWebScriptExpression.Compile: TFunc<TValue>;
var
  Expression: IExpression;
  LProgram: IdwsProgram;
begin
  Expression := Self;
  if FText <> EmptyStr then
  begin
    FScope.Value := FExpression.Execute();
    if not FScope.Value.IsEmpty then
    begin
      // very simple check if the expression is not an assignment already
      if ContainsText(FText, ':=') then
      begin
        LProgram := TDelphiWebScriptConnector.Compile(FText, FScope);
      end
      else
      begin
        LProgram := TDelphiWebScriptConnector.Compile('var Result := ' + FText, FScope);
      end;
      if not LProgram.Msgs.HasErrors then
      begin
        Result :=
          function: TValue
          var
            LExecution: IdwsProgramExecution;
            LVariant: TdwsRTTIVariant;
          begin
            if Assigned(Expression) then;

            Result := TValue.Empty;
            LExecution := LProgram.BeginNewExecution;
            try
              LExecution.RunProgram(0);
              if not ContainsText(FText, ':=') then
              begin
                Result := TValue.FromVariant(LExecution.Info.Vars['Result'].Value);
                if (Result.Kind = tkInterface) and ((Result.AsInterface as TObject) is TdwsRTTIVariant) then
                begin
                  LVariant := Result.AsInterface as TdwsRTTIVariant;
                  TValue.Make(@LVariant.Instance, LVariant.RTTIType.Handle, Result);
                end;
              end;
            finally
              LExecution.EndProgram;
            end;
          end;
        Exit;
      end;
    end
  end;
  // return empty
  Result :=
    function: TValue
    begin
      Result := TValue.Empty;
    end;
end;

function TDelphiWebScriptExpression.ToString: string;
begin
  Result := FText;
end;

end.
