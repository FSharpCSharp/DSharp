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

unit System.Bindings.Controls;

interface

uses
  ComCtrls,
  CommCtrl,
  Controls,
  ExtCtrls,
  Messages,
  StdCtrls,
  System.Bindings,
  System.Events;

type
  TCheckBox = class(StdCtrls.TCheckBox, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Click; override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TColorBox = class(ExtCtrls.TColorBox, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Change; override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TComboBox = class(StdCtrls.TComboBox, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Change; override;
    procedure Select; override;

    procedure SetItemIndex(const Value: Integer); override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TDateTimePicker = class(ComCtrls.TDateTimePicker, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Change; override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TEdit = class(StdCtrls.TEdit, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Change; override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TMonthCalendar = class(ComCtrls.TMonthCalendar, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CNNotify(var Message: TWMNotifyMC); message CN_NOTIFY;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TRadioButton = class(StdCtrls.TRadioButton, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure SetChecked(Value: Boolean); override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TRadioGroup = class(ExtCtrls.TRadioGroup, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Click; override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

  TTrackBar = class(ComCtrls.TTrackBar, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure Changed; override;
  public
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  end;

implementation

{ TCheckBox }

procedure TCheckBox.Click;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Checked', utPropertyChanged);
  FOnPropertyChanged.Invoke(Self, 'State', utPropertyChanged);
end;

procedure TCheckBox.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Checked', utLostFocus);
  FOnPropertyChanged.Invoke(Self, 'State', utLostFocus);
end;

function TCheckBox.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TColorBox }

procedure TColorBox.Change;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Selected', utPropertyChanged);
end;

procedure TColorBox.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Selected', utLostFocus);
end;

function TColorBox.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TComboBox }

procedure TComboBox.Change;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utPropertyChanged);
end;

procedure TComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'ItemIndex', utLostFocus);
end;

function TComboBox.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TComboBox.Select;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'ItemIndex', utPropertyChanged);
end;

procedure TComboBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'ItemIndex', utPropertyChanged);
end;

{ TDateTimePicker }

procedure TDateTimePicker.Change;
begin
  inherited;
  case Kind of
    dtkDate: FOnPropertyChanged.Invoke(Self, 'Date', utPropertyChanged);
    dtkTime: FOnPropertyChanged.Invoke(Self, 'Time', utPropertyChanged);
  end;
end;

procedure TDateTimePicker.CMExit(var Message: TCMExit);
begin
  inherited;
  case Kind of
    dtkDate: FOnPropertyChanged.Invoke(Self, 'Date', utLostFocus);
    dtkTime: FOnPropertyChanged.Invoke(Self, 'Time', utLostFocus);
  end;
end;

function TDateTimePicker.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TEdit }

procedure TEdit.Change;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utPropertyChanged);
end;

procedure TEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utLostFocus);
end;

function TEdit.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TMonthCalendar }

procedure TMonthCalendar.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Date', utLostFocus);
end;

procedure TMonthCalendar.CNNotify(var Message: TWMNotifyMC);
begin
  inherited;
  case Message.NMHdr.code of
    MCN_SELCHANGE:
      FOnPropertyChanged.Invoke(Self, 'Date', utPropertyChanged);
  end;
end;

function TMonthCalendar.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TRadioButton }

procedure TRadioButton.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Checked', utLostFocus);
end;

function TRadioButton.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TRadioButton.SetChecked(Value: Boolean);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Checked', utPropertyChanged);
end;

{ TRadioGroup }

procedure TRadioGroup.Click;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'ItemIndex', utPropertyChanged);
end;

procedure TRadioGroup.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'ItemIndex', utLostFocus);
end;

function TRadioGroup.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TTrackBar }

procedure TTrackBar.Changed;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Position', utPropertyChanged);
end;

procedure TTrackBar.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Position', utLostFocus);
end;

function TTrackBar.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

end.
