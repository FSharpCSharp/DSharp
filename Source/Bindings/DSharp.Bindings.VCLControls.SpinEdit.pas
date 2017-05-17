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

unit DSharp.Bindings.VCLControls.SpinEdit;

interface

uses
  Classes,
  Controls,
  Spin,
  DSharp.Bindings.Notifications;

// while we are here, we also fix that crappy bug from GetValue that is causing
// exceptions every time we don't enter a valid name

type
  TSpinEdit = class(Spin.TSpinEdit, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure DownClick (Sender: TObject); override;
    procedure UpClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: Integer read GetValue write SetValue;
  end;

implementation

uses
  SysUtils,
  Windows;

{ TSpinEdit }

constructor TSpinEdit.Create(AOwner: TComponent);
begin
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  inherited;
end;

procedure TSpinEdit.DownClick(Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else Value := Value - Increment;
end;

function TSpinEdit.GetValue: Integer;
begin
  Result := StrToIntDef(Text, MinValue);
end;

procedure TSpinEdit.SetValue(const Value: Integer);
begin
  inherited Value := Value;
end;

procedure TSpinEdit.UpClick(Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else Value := Value + Increment;
end;

procedure TSpinEdit.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
  NotifyPropertyChanged.DoPropertyChanged('Value');
end;

procedure TSpinEdit.CMExit(var Message: TCMExit);
begin
  SetValue(GetValue);
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
  NotifyPropertyChanged.DoPropertyChanged('Value', utLostFocus);
end;

end.
