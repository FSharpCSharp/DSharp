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
  Classes,
  DSharp.Aspects.Logging,
  DSharp.Bindings.Notifications,
  DSharp.Bindings.Validations,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.Core.Events,
  DSharp.Core.Validations,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.Validations,
  DSharp.PresentationModel.WindowManager;

type
  [InheritedExport]
  [Logging]
  [Validation(TDataErrorValidationRule)]
  {$RTTI EXPLICIT METHODS([vcProtected..vcPublished])}
  TViewModelBase = class(TComponent, IInterface, INotifyPropertyChanged,
    IValidatable, IDataErrorInfo, ICanClose, IClose, IHaveDisplayName)
  private
    FPropertyChanged: Event<TPropertyChangedEvent>;
    FThrowOnInvalidPropertyName: Boolean;
    FParent: TObject;
    FRefCount: Integer;
    FValidationErrors: IList<IValidationResult>;
    [Import]
    FWindowManager: IWindowManager;

    function GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
    function GetValidationErrors: IList<IValidationResult>;
  protected
    // IInterface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);

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
    function Validate: Boolean; virtual;

    property WindowManager: IWindowManager read FWindowManager;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(AOwner: TComponent); overload; override;
    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;

    procedure VerifyPropertyName(const APropertyName: string);
    property DisplayName: string read GetDisplayName;
    property Error: string read GetError;
    property Parent: TObject read FParent write FParent;
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
  Create(nil);
end;

constructor TViewModelBase.Create(AOwner: TComponent);
begin
  inherited;
  FValidationErrors := TList<IValidationResult>.Create();
end;

procedure TViewModelBase.AfterConstruction;
begin
  Dec(FRefCount);
end;

function TViewModelBase.CanClose: Boolean;
begin
  Result := FValidationErrors.Count = 0;
end;

procedure TViewModelBase.Close;
begin

end;

procedure TViewModelBase.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
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

function TViewModelBase.GetOnPropertyChanged: IEvent<TPropertyChangedEvent>;
begin
  Result := FPropertyChanged;
end;

function TViewModelBase.GetValidationErrors: IList<IValidationResult>;
begin
  Result := FValidationErrors;
end;

class function TViewModelBase.NewInstance: TObject;
var
  LInterfaceTable: PInterfaceTable;
begin
  Result := inherited NewInstance;
  LInterfaceTable := Result.GetInterfaceTable;
  if Assigned(LInterfaceTable) and (LInterfaceTable.EntryCount > 0) then
    TViewModelBase(Result).FRefCount := 1
  else
    TViewModelBase(Result).FRefCount := 2;
end;

function TViewModelBase.Validate: Boolean;
var
  LAttribute: ValidationAttribute;
  LValidation: IValidationRule;
  LValidationResult: IValidationResult;
begin
  FValidationErrors.Clear();

  Result := True;

  for LAttribute in GetType.GetCustomAttributes<ValidationAttribute>(True) do
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
{$IFDEF DEBUG}
var
  LMessage: string;
{$ENDIF}
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

function TViewModelBase._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TViewModelBase._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result = 0 then
    Destroy;
end;

end.
