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

unit DSharp.PresentationModel.EditableViewModelBase;

interface

uses
  Classes,
  DSharp.Bindings,
  DSharp.Core.PropertyChangedBase,
  DSharp.Core.Validations;

type
  TEditableViewModelBase<T: TPersistent, constructor> = class(TPropertyChangedBase,
    IEditable, IValidatable)
  private
    FCache: T;
    FItem: T;
    FValidationErrors: TArray<TValidationResult>;
    function GetValidationErrors: TArray<TValidationResult>;
    procedure SetItem(const Value: T);
  protected
    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;

    property Item: T read FItem write SetItem;
  public
    destructor Destroy; override;

    procedure Cancel; virtual;
    procedure Save; virtual;

    function Validate: Boolean;
    property ValidationErrors: TArray<TValidationResult> read GetValidationErrors;
  end;

implementation

uses
  SysUtils;

{ TEditableViewModelBase<T> }

procedure TEditableViewModelBase<T>.BeginEdit;
begin
  if Assigned(FItem) then
  begin
    if not Assigned(FCache) then
    begin
      FCache := T.Create();
    end;
    FCache.Assign(FItem);
  end;
end;

procedure TEditableViewModelBase<T>.Cancel;
begin
  CancelEdit;
end;

procedure TEditableViewModelBase<T>.CancelEdit;
begin
  if Assigned(FCache) and Assigned(FItem) then
  begin
    FItem.Assign(FCache);
    FreeAndNil(FCache);
  end;
end;

destructor TEditableViewModelBase<T>.Destroy;
begin
  FreeAndNil(FCache);
  inherited;
end;

procedure TEditableViewModelBase<T>.EndEdit;
begin
  FreeAndNil(FCache);
end;

function TEditableViewModelBase<T>.GetValidationErrors: TArray<TValidationResult>;
begin
  Result := FValidationErrors;
end;

procedure TEditableViewModelBase<T>.Save;
begin
  EndEdit();
end;

procedure TEditableViewModelBase<T>.SetItem(const Value: T);
begin
  CancelEdit();
  FItem := Value;
  BeginEdit();
end;

function TEditableViewModelBase<T>.Validate: Boolean;
begin

end;

end.
