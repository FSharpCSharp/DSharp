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

unit DSharp.PresentationModel.ViewModelBase;

interface

uses
  DSharp.Bindings.Validations,
  DSharp.Collections,
  DSharp.Core.PropertyChangedBase,
  DSharp.Core.Validations,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.Validations;

type
  [Validation(TDataErrorValidationRule)]
  TViewModelBase = class(TPropertyChangedBase,
    IValidatable, IDataErrorInfo, ICanClose, IClose, IHaveDisplayName)
  private
    FThrowOnInvalidPropertyName: Boolean;
    FValidationErrors: IList<IValidationResult>;

    function GetValidationErrors: IList<IValidationResult>;
  protected
    // ICanClose
    function CanClose: Boolean; virtual;

    // IClose
    procedure Close; virtual;

    // IDataErrorInfo
    function GetError: string; virtual;
    function GetItem(const AName: string): string; virtual;

    // IHaveDisplayName
    function GetDisplayName: string; virtual;

    // IValidatable
    function Validate: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure VerifyPropertyName(const APropertyName: string);
    property DisplayName: string read GetDisplayName;
    property Error: string read GetError;
    property ThrowOnInvalidPropertyName: Boolean
      read FThrowOnInvalidPropertyName write FThrowOnInvalidPropertyName;
    property ValidationErrors: IList<IValidationResult> read GetValidationErrors;
  end;

implementation

uses
  DSharp.Core.Reflection,
  Rtti,
  SysUtils;

{ TViewModelBase }

constructor TViewModelBase.Create;
begin
  FValidationErrors := TList<IValidationResult>.Create();
end;

destructor TViewModelBase.Destroy;
begin
  FValidationErrors.Free();
  inherited;
end;

function TViewModelBase.CanClose: Boolean;
begin
  Result := FValidationErrors.Count = 0;
end;

procedure TViewModelBase.Close;
begin

end;

function TViewModelBase.GetDisplayName: string;
begin
  Result := ClassName;
end;

function TViewModelBase.GetError: string;
begin
  Result := '';
end;

function TViewModelBase.GetItem(const AName: string): string;
begin
  Result := '';
end;

function TViewModelBase.GetValidationErrors: IList<IValidationResult>;
begin
  Result := FValidationErrors;
end;

function TViewModelBase.Validate: Boolean;
var
  LAttribute: ValidationAttribute;
  LValidation: IValidationRule;
  LValidationResult: IValidationResult;
begin
  FValidationErrors.Clear();

  Result := True;

  for LAttribute in GetType.GetAttributesOfType<ValidationAttribute>() do
  begin
    LValidation := LAttribute.ValidationRuleClass.Create();
    LValidationResult := LValidation.Validate(TValue.From<TObject>(Self));
    if not LValidationResult.IsValid then
    begin
      FValidationErrors.Add(LValidationResult);
      Result := False;
      Break;
    end;
  end;
end;

procedure TViewModelBase.VerifyPropertyName(const APropertyName: string);
var
  LMessage: string;
begin
{$IFDEF DEBUG}
  if GetProperty(APropertyName) = nil then
  begin
    LMessage := 'Invalid property name: ' + APropertyName;
    if FThrowOnInvalidPropertyName then
      raise Exception.Create(LMessage)
//    else
//      Logging.LogMessage(LMessage);
  end;
{$ENDIF}
end;

end.
