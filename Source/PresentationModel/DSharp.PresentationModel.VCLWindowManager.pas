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

unit DSharp.PresentationModel.VCLWindowManager;

interface

uses
  Classes,
  Controls,
  Dialogs,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.WindowManager,
  Forms,
  Messages;

type
  [PartCreationPolicy(cpShared)]
  TWindowManager = class(TInterfacedObject, IWindowManager)
  private
    FRunning: Boolean;
    function WindowHook(var Message: TMessage): Boolean;
  protected
    function CreateWindow(Model: TObject; IsDialog: Boolean): TForm;
    function EnsureWindow(Model: TObject; View: TControl): TForm;
  public
    constructor Create;
    destructor Destroy; override;

    function InputBox(const ACaption, APrompt, ADefault: string;
      AShowPasswordChar: Boolean = False): string;
    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: LongInt = 0): Integer;

    function ShowDialog(Model: IInterface): Integer; overload;
    function ShowDialog(Model: TObject): Integer; overload;
    procedure ShowWindow(Model: IInterface); overload;
    procedure ShowWindow(Model: TObject); overload;
  end;

implementation

uses
  DSharp.Bindings,
  DSharp.PresentationModel.ChildForm,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.VCLConventionManager,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.PresentationModel.WindowConductor,
  SysUtils,
  Windows;

{ TWindowManager }

constructor TWindowManager.Create;
begin
  // Following line should be here -
  // but if this line is missing in project file the runtime themes cannot be set
//  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.HookMainWindow(WindowHook);
end;

function TWindowManager.CreateWindow(Model: TObject; IsDialog: Boolean): TForm;
var
  LView: TControl;
  LViewClass: TClass;
  LWindow: TForm;
begin
  LViewClass := ViewLocator.FindViewType(Model.ClassType);

  if not FRunning and LViewClass.InheritsFrom(TForm) then
  begin
    Application.CreateForm(TComponentClass(LViewClass), LWindow);
    LView := LWindow;
  end
  else
  begin
    LView := ViewLocator.GetOrCreateViewType(Model.ClassType) as TControl;
    LWindow := EnsureWindow(Model, LView);
  end;

  TWindowConductor.Create(Model, LWindow);
  ViewModelBinder.Bind(Model, LView);

  if Supports(Model, IHaveDisplayName) then
  begin
    FindBindingGroup(LView).AddBinding(
      Model, 'DisplayName', LWindow, 'Caption', bmOneWay);
  end;

  Result := LWindow;
end;

destructor TWindowManager.Destroy;
begin
  Application.UnhookMainWindow(WindowHook);
end;

function TWindowManager.EnsureWindow(Model: TObject; View: TControl): TForm;
var
  LForm: TChildForm;
begin
  if View is TForm then
  begin
    Result := View as TForm;
  end
  else
  begin
    if not FRunning then
    begin
      Application.CreateForm(TChildForm, LForm);
      LForm.BorderIcons := [biSystemMenu..biMaximize];
    end
    else
    begin
      LForm := TChildForm.Create(nil);
      LForm.BorderIcons := [biSystemMenu];
      LForm.Position := poOwnerFormCenter;
    end;

    LForm.Content := View;
    Result := LForm;
  end;
end;

function TWindowManager.InputBox(const ACaption, APrompt, ADefault: string;
  AShowPasswordChar: Boolean): string;
begin
  if AShowPasswordChar then
    PostMessage(Application.Handle, WM_USER + EM_SETPASSWORDCHAR, 0, 0);
  Result := Dialogs.InputBox(ACaption, APrompt, ADefault);
end;

function TWindowManager.MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer): Integer;
begin
{$IF COMPILERVERSION < 23}
  Result := Dialogs.MessageDlg(Msg, Dialogs.TMsgDlgType(DlgType),
    Dialogs.TMsgDlgButtons(Buttons), HelpCtx);
{$ELSE}
  Result := Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx);
{$IFEND}
end;

function TWindowManager.ShowDialog(Model: IInterface): Integer;
begin
  Result := ShowDialog(Model as TObject);
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

procedure TWindowManager.ShowWindow(Model: IInterface);
begin
  ShowWindow(Model as TObject);
end;

procedure TWindowManager.ShowWindow(Model: TObject);
var
  LWindow: TForm;
begin
  LWindow := CreateWindow(Model, False);

  if not FRunning then
  begin
    LWindow.Caption := Application.Title;
    LWindow.Position := poScreenCenter;
    FRunning := True;
    Application.Run();
  end
  else
  begin
    LWindow.BorderIcons := [biSystemMenu];
    LWindow.Position := poOwnerFormCenter;

    LWindow.Show();
  end;
end;

function TWindowManager.WindowHook(var Message: TMessage): Boolean;
var
  LInputForm, LEdit: HWND;
begin
  case Message.Msg of
    WM_USER + EM_SETPASSWORDCHAR:
    begin
      LInputForm := Screen.Forms[0].Handle;
      if LInputForm <> 0 then
      begin
        LEdit := FindWindowEx(LInputForm, 0, 'TEdit', nil);
        SendMessage(LEdit, EM_SETPASSWORDCHAR, Ord('*'), 0);
      end;
      Result := True;
    end
  else
    Result := False;
  end;
end;

initialization
  TWindowManager.ClassName;

end.
