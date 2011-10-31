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

unit DSharp.PresentationModel.TabSheetConductor;

interface

uses
  Classes,
  DSharp.Bindings.VCLControls,
  Forms;

type
  TTabSheetConductor = class(TComponent)
  private
    FModel: TObject;
    FView: TTabSheet;
    procedure Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    constructor Create(Model: TObject; View: TTabSheet); reintroduce;
  end;

implementation

uses
  Controls,
  DSharp.Core.Editable,
  DSharp.Core.Validations,
  DSharp.PresentationModel.Screen,
  SysUtils;

{ TTabSheetConductor }

procedure TTabSheetConductor.Close(Sender: TObject;
  var CloseAction: TCloseAction);
var
  LClose: IClose;
  LEditable: IEditable;
begin
  if Supports(FModel, IEditable, LEditable) then
  begin
    case FView.ModalResult of
      mrOk: LEditable.EndEdit();
      mrCancel: LEditable.CancelEdit();
    end;
  end;

  if Supports(FModel, IClose, LClose) then
  begin
    LClose.Close();
  end;
end;

procedure TTabSheetConductor.CloseQuery(Sender: TObject; var CanClose: Boolean);
var
  LCanClose: ICanClose;
  LValidatable: IValidatable;
begin
  if (FView.ModalResult = mrOk) then
  begin
    if Supports(FModel, IValidatable, LValidatable) then
    begin
      LValidatable.Validate();
    end;

    if Supports(FModel, ICanClose, LCanClose) then
    begin
      CanClose := LCanClose.CanClose();
    end;
  end;
end;

constructor TTabSheetConductor.Create(Model: TObject; View: TTabSheet);
var
  LCanClose: ICanClose;
  LClose: IClose;
begin
  inherited Create(View);

  FModel := Model;
  FView := View;

  if Supports(FModel, ICanClose, LCanClose) then
  begin
    FView.OnCloseQuery := CloseQuery;
  end;

  if Supports(FModel, IClose, LClose) then
  begin
    FView.OnClose := Close;
  end;
end;

end.
