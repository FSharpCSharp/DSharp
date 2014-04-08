(*
  Copyright (c) 2012-2014, Stefan Glienke
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

unit DSharp.Interception.AttributeDrivenPolicy;

interface

uses
  DSharp.Interception,
  DSharp.Interception.InjectionPolicy,
  DSharp.Interception.MethodImplementationInfo,
  Rtti;

type
  TAttributeDrivenPolicy = class(TInjectionPolicy)
  private
    FAttributeMatchingRule: IMatchingRule;
  protected
    function DoesMatch(Member: TMethodImplementationInfo): Boolean; override;
    function DoGetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>; override;
  public
    constructor Create;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Core.Utils,
  DSharp.Interception.AttributeDrivenPolicyMatchingRule;

{ TAttributeDrivenPolicy }

constructor TAttributeDrivenPolicy.Create;
begin
  inherited Create('Attribute Driven Policy');
  FAttributeMatchingRule := TAttributeDrivenPolicyMatchingRule.Create;
end;

function TAttributeDrivenPolicy.DoesMatch(Member: TMethodImplementationInfo): Boolean;
var
  matchesInterface: Boolean;
  matchesImplementation: Boolean;
begin
  matchesInterface := IfThen(Assigned(Member.InterfaceMethodInfo),
    FAttributeMatchingRule.Matches(Member.InterfaceMethodInfo));
  matchesImplementation := FAttributeMatchingRule.Matches(Member.ImplementationMethodInfo);

  Result := matchesInterface or matchesImplementation;
end;

function TAttributeDrivenPolicy.DoGetHandlersFor(
  Member: TMethodImplementationInfo): TArray<ICallHandler>;
var
  attribute: HandlerAttribute;
begin
  if Assigned(Member.InterfaceMethodInfo) then
  begin
    for attribute in Member.InterfaceMethodInfo.GetAllAttributes<HandlerAttribute>(True) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute.CreateHandler;
    end;
  end;
  if Assigned(Member.ImplementationMethodInfo) then
  begin
    for attribute in Member.ImplementationMethodInfo.GetAllAttributes<HandlerAttribute>(True) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute.CreateHandler;
    end;
  end;
end;

end.
