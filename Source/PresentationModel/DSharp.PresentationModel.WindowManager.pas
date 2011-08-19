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

unit DSharp.PresentationModel.WindowManager;

interface

uses
  Classes,
  Controls,
  DSharp.ComponentModel.Composition,
  Forms;

type
  [InheritedExport]
  IWindowManager = interface
    ['{61AB99D7-E09D-4D94-B1EC-EA154959809D}']
    function ShowDialog(ARootModel: TObject): Integer;
    procedure ShowWindow(ARootModel: TObject);
  end;

  [PartCreationPolicy(cpShared)]
  TWindowManager = class(TInterfacedObject, IWindowManager)
  private
    FRunning: Boolean;
  protected
    function CreateWindow(ARootModel: TObject; AIsDialog: Boolean): TForm;
    function EnsureWindow(AModel: TObject; AView: TControl): TForm;
  public
    constructor Create;

    function ShowDialog(ARootModel: TObject): Integer;
    procedure ShowWindow(ARootModel: TObject);
  end;

implementation

uses
  DSharp.Bindings,
  DSharp.PresentationModel.ChildForm,
  DSharp.PresentationModel.Screen,
  DSharp.PresentationModel.ViewLocator,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.PresentationModel.WindowConductor,
  SysUtils;

{ TWindowManager }

constructor TWindowManager.Create;
begin
  // Following line should be here -
  // but if this line is missing in project file the runtime themes cannot be set
//  Application.Initialize;
  Application.MainFormOnTaskbar := True;
end;

function TWindowManager.CreateWindow(ARootModel: TObject; AIsDialog: Boolean): TForm;
var
  LView: TControl;
  LWindow: TForm;
begin
  LView := ViewLocator.GetOrCreateViewType(ARootModel.ClassType);
  LWindow := EnsureWindow(ARootModel, LView);
  TWindowConductor.Create(ARootModel, LWindow);
  ViewModelBinder.Bind(ARootModel, LView);

  if Supports(ARootModel, IHaveDisplayName) then
  begin
    TBinding.Create(ARootModel, 'DisplayName', LWindow, 'Caption', bmOneWay);
  end;

  Result := LWindow;
end;

function TWindowManager.EnsureWindow(AModel: TObject; AView: TControl): TForm;
var
  LForm: TChildForm;
begin
  if AView is TForm then
  begin
    Result := AView as TForm;
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

    LForm.Content := AView;
    Result := LForm;
  end;
end;

function TWindowManager.ShowDialog(ARootModel: TObject): Integer;
var
  LWindow: TForm;
begin
  LWindow := CreateWindow(ARootModel, True);

  try
    Result := LWindow.ShowModal();
  finally
    LWindow.Free();
  end;
end;

procedure TWindowManager.ShowWindow(ARootModel: TObject);
var
  LWindow: TForm;
begin
  LWindow := CreateWindow(ARootModel, False);

  if not FRunning then
  begin
    LWindow.Caption := Application.Title;
    LWindow.Position := poScreenCenter;
    FRunning := True;
    Application.Run();
    Application.MainForm.Free();
  end
  else
  begin
    LWindow.BorderIcons := [biSystemMenu];
    LWindow.Position := poOwnerFormCenter;

    LWindow.Show();
  end;
end;

initialization
  TWindowManager.ClassName;

end.
