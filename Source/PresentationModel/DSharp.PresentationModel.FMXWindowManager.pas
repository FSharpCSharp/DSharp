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

unit DSharp.PresentationModel.FMXWindowManager;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.WindowManager,
  FMX.Dialogs,
  FMX.Forms,
  FMX.Types;

type
  [PartCreationPolicy(cpShared)]
  TWindowManager = class(TInterfacedObject, IWindowManager)
  private
    FRunning: Boolean;
  protected
    function CreateWindow(Model: TObject; IsDialog: Boolean): TForm;
  public
    constructor Create;

    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: LongInt = 0): Integer;

    function ShowDialog(Model: TObject): Integer;
    procedure ShowWindow(Model: TObject);
  end;

implementation

uses
  Classes,
  DSharp.Bindings,
  DSharp.PresentationModel.FMXConventionManager,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder,
  System.UITypes,
  SysUtils;
//  DSharp.PresentationModel.WindowConductor;

{ TWindowManager }

constructor TWindowManager.Create;
begin
  Application.MainFormOnTaskbar := True;
end;

function TWindowManager.CreateWindow(Model: TObject; IsDialog: Boolean): TForm;
var
  LView: TForm;
begin
  // since FMX does not support frames (yet?) views are always forms

  if not FRunning then
  begin
    Application.CreateForm(TComponentClass(ViewLocator.FindViewType(Model.ClassType)), LView);
    Application.RealCreateForms;
  end
  else
  begin
    LView := ViewLocator.GetOrCreateViewType(Model.ClassType) as TForm;
  end;

  // TODO: Create FMX WindowConductor
//  TWindowConductor.Create(Model, LView);
  ViewModelBinder.Bind(Model, LView);

  if Supports(Model, IHaveDisplayName) then
  begin
    TBinding.Create(Model, 'DisplayName', LView, 'Caption', bmOneWay);
  end;

  Result := LView;
end;

function TWindowManager.MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer): Integer;
begin
  Result := FMX.Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx);
end;

function TWindowManager.ShowDialog(Model: TObject): Integer;
var
  LWindow: TForm;
begin
  LWindow := CreateWindow(Model, True);

  try
    Result := LWindow.ShowModal();
  finally
    LWindow.Free();
  end;
end;

procedure TWindowManager.ShowWindow(Model: TObject);
var
  LWindow: TForm;
begin
  LWindow := CreateWindow(Model, False);

  if not FRunning then
  begin
    LWindow.Caption := Application.Title;
    LWindow.Position := TFormPosition.poScreenCenter;
    FRunning := True;
    Application.Run();
  end
  else
  begin
    LWindow.BorderIcons := [TBorderIcon.biSystemMenu];
    LWindow.Position := TFormPosition.poOwnerFormCenter;

    LWindow.Show();
  end;
end;

initialization
  TWindowManager.ClassName;

end.
