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

unit DSharp.Core.Validations;

interface

uses
  Rtti,
  SysUtils;

type
  TValidationStep = (vsRawProposedValue, vsConvertedProposedValue);//, vsUpdatedValue, vsCommittedValue);

  TValidationResult = record
  strict private
  	FErrorContent: string;
    FIsValid: Boolean;
  public
    constructor Create(AIsValid: Boolean; AErrorContent: string);
    class function ValidResult: TValidationResult; static;
    property ErrorContent: string read FErrorContent;
    property IsValid: Boolean read FIsValid;
  end;

  TValidationRule = class abstract
  private
    FValidatesOnTargetUpdated: Boolean;
    FValidationStep: TValidationStep;
  public
    function Validate(AValue: TValue): TValidationResult; virtual;

    property ValidatesOnTargetUpdated: Boolean read FValidatesOnTargetUpdated
      write FValidatesOnTargetUpdated;
    property ValidationStep: TValidationStep read FValidationStep write FValidationStep;
  end;

  TValidationEvent = procedure(Sender: TObject;
    AValidationRule: TValidationRule; AValidationResult: TValidationResult) of object;

implementation

{ TValidationResult }

constructor TValidationResult.Create(AIsValid: Boolean; AErrorContent: string);
begin
  FIsValid := AIsValid;
  FErrorContent := AErrorContent;
end;

class function TValidationResult.ValidResult: TValidationResult;
begin
  Result := TValidationResult.Create(True, '');
end;

{ TValidationRule }

function TValidationRule.Validate(AValue: TValue): TValidationResult;
begin
  Result := TValidationResult.ValidResult();
end;

end.
