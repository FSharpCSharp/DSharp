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
  Rtti;

type
  TDelphiWebScriptExpression = class(TExpression)
  private
    FText: string;
    FObjects: TStrings;

    function BuildScript: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BindObject(AObject: TObject; const AVariableName: string = '');
    function Compile: TValue; override;
    property Text: string read FText write FText;
  end;

implementation

uses
  DSharp.DelphiWebScript.Connector,
  dwsExprs,
  dwsRTTIConnector,
  SysUtils;

{ TDelphiWebScriptExpression }

constructor TDelphiWebScriptExpression.Create;
begin
  FObjects := TStringList.Create();
end;

destructor TDelphiWebScriptExpression.Destroy;
begin
  FObjects.Free();
end;

procedure TDelphiWebScriptExpression.BindObject(AObject: TObject;
  const AVariableName: string);
var
  LVariableName: string;
begin
  LVariableName := AVariableName;
  if (LVariableName = '') and (AObject is TComponent) then
  begin
    LVariableName := TComponent(AObject).Name;
  end;
  FObjects.AddObject(LVariableName, AObject);
end;

function TDelphiWebScriptExpression.BuildScript: string;
var
  i: Integer;
begin
  Result := Format('var Result := %s', [FText]);
  for i := 0 to Pred(FObjects.Count) do
  begin
    Result := Format('var %s: RttiVariant;', [FObjects[i]]) + Result;
  end;
end;

function TDelphiWebScriptExpression.Compile: TValue;
var
  i: Integer;
  LExecution: IdwsProgramExecution;
  LProgram: IdwsProgram;
begin
  LProgram := TDelphiWebScriptConnector.Compile(BuildScript());
  if not LProgram.Msgs.HasErrors then
  begin
    LExecution := LProgram.BeginNewExecution;
    try
      for i := 0 to Pred(FObjects.Count) do
      begin
        LExecution.Info.Vars[FObjects[i]].Value := TdwsRTTIVariant.FromObject(FObjects.Objects[i]);
      end;
      LExecution.RunProgram(0);
      Result := TValue.FromVariant(LExecution.Info.Vars['Result'].Value);
    finally
      LExecution.EndProgram;
    end;
  end;
end;

end.
