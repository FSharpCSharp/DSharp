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

unit DSharp.Interception.InjectionPolicy;

interface

uses
  DSharp.Interception,
  DSharp.Interception.MethodImplementationInfo,
  Rtti;

type
  TInjectionPolicy = class(TInterfacedObject, IInjectionPolicy)
  private
    FName: string;
    FDoesNotHaveNoPoliciesAttributeRule: IMatchingRule;
    function GetName: string;
  protected
    function DoesMatch(Member: TMethodImplementationInfo): Boolean; virtual; abstract;
    function DoGetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>; virtual; abstract;
  public
    constructor Create; overload;
    constructor Create(const Name: string); overload;

    function GetHandlersFor(Member: TMethodImplementationInfo): TArray<ICallHandler>; virtual;
    function Matches(Member: TMethodImplementationInfo): Boolean;

    property Name: string read GetName;
  end;

implementation

uses
  DSharp.Interception.ApplyNoPoliciesMatchingRule;

{ TInjectionPolicy }

constructor TInjectionPolicy.Create;
begin
  Create('Unnamed policy');
end;

constructor TInjectionPolicy.Create(const Name: string);
begin
  FName := Name;
  FDoesNotHaveNoPoliciesAttributeRule := TApplyNoPoliciesMatchingRule.Create;
end;

function TInjectionPolicy.GetHandlersFor(
  Member: TMethodImplementationInfo): TArray<ICallHandler>;
begin
  Result := DoGetHandlersFor(Member);
end;

function TInjectionPolicy.GetName: string;
begin
  Result := FName;
end;

function TInjectionPolicy.Matches(Member: TMethodImplementationInfo): Boolean;
begin
  Result := DoesMatch(Member);
end;

end.
