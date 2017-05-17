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
  DSharp.Core.Editable,
  DSharp.PresentationModel.ViewModelBase;

type
  TEditAction = (eaCancel, eaSave);

  TEditableViewModelBase<T: TPersistent, constructor> = class(TViewModelBase,
    IEditable)
  private
    FCache: T;
    FItem: T;
    procedure SetItem(const Value: T);
  protected
    // IEditable
    procedure BeginEdit;
    procedure CancelEdit;
    procedure EndEdit;

    procedure Close; override;

    property Item: T read FItem write SetItem;
  public
    destructor Destroy; override;

    property Item[const AName: string]: string read GetItem; default;
  end;

implementation

{ TEditableViewModelBase<T> }

procedure TEditableViewModelBase<T>.BeginEdit;
begin
  if Assigned(FItem) then
  begin
    if not Assigned(FCache) then
    begin
      FCache := FItem;
      FItem := T.Create();
    end;
    FItem.Assign(FCache);
  end;
end;

procedure TEditableViewModelBase<T>.CancelEdit;
begin
  if Assigned(FCache) and Assigned(FItem) then
  begin
    FItem.Free();
    FItem := FCache;
    TObject(FCache) := nil;
  end;
end;

procedure TEditableViewModelBase<T>.Close;
begin
  CancelEdit();
end;

destructor TEditableViewModelBase<T>.Destroy;
begin
  CancelEdit();
  inherited;
end;

procedure TEditableViewModelBase<T>.EndEdit;
begin
  FCache.Assign(FItem);
  FItem.Free();
  FItem := FCache;
  TObject(FCache) := nil;
end;

procedure TEditableViewModelBase<T>.SetItem(const Value: T);
begin
  CancelEdit();
  FItem := Value;
  BeginEdit();
end;

end.
