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

unit System.Bindings.Controls.VCL;

interface

uses
  Classes,
  ComCtrls,
  CommCtrl,
  Controls,
  ExtCtrls,
  Forms,
  Generics.Collections,
  Messages,
  StdCtrls,
  System.Bindings,
  System.Bindings.Collections,
  System.Data.Templates,
  System.Data.Templates.Default,
  System.Events,
  SysUtils;

type
  TCheckBox = class(StdCtrls.TCheckBox, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Click; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TColorBox = class(ExtCtrls.TColorBox, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Change; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TComboBox = class(StdCtrls.TComboBox, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
  protected
    procedure Change; override;
    procedure Select; override;

    procedure SetItemIndex(const Value: Integer); override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  published
    property Text: TCaption read GetText write SetText;
  end;

  TDateTimePicker = class(ComCtrls.TDateTimePicker, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Change; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TEdit = class(StdCtrls.TEdit, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Change; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TForm = class(Forms.TForm, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TFrame = class(Forms.TFrame, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure SetBindingSource(const Value: TObject);
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    property BindingSource: TObject read FBindingSource write SetBindingSource;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TGroupBox = class(StdCtrls.TGroupBox, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure SetBindingSource(const Value: TObject);
  public
    property BindingSource: TObject read FBindingSource write SetBindingSource;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TListBox = class(StdCtrls.TListBox, INotifyPropertyChanged, ICollectionView)
  private
    FFilter: TPredicate<TObject>;
    FItemsSource: TList<TObject>;
    FItemTemplate: IDataTemplate;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetCurrentItem: TObject;
    function GetFilter: TPredicate<TObject>;
    function GetItemsSource: TList<TObject>;
    function GetItemTemplate: IDataTemplate;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure SetCurrentItem(const Value: TObject);
    procedure SetFilter(const Value: TPredicate<TObject>);
    procedure SetItemsSource(const Value: TList<TObject>);
    procedure SetItemTemplate(const Value: IDataTemplate);

    procedure UpdateItems(AClearItems: Boolean = False);
  protected
    procedure Click; override;
    procedure OnSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionNotification);
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    constructor Create(AOwner: TComponent); override;
    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property Filter: TPredicate<TObject> read GetFilter write SetFilter;
    property ItemsSource: TList<TObject> read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent> read FOnCollectionChanged;
  end;

  TMemo = class(StdCtrls.TMemo, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Change; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TMonthCalendar = class(ComCtrls.TMonthCalendar, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CNNotify(var Message: TWMNotifyMC); message CN_NOTIFY;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TPanel = class(ExtCtrls.TPanel, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure SetBindingSource(const Value: TObject);
  public
    property BindingSource: TObject read FBindingSource write SetBindingSource;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TRadioButton = class(StdCtrls.TRadioButton, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure SetChecked(Value: Boolean); override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TRadioGroup = class(ExtCtrls.TRadioGroup, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Click; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TTrackBar = class(ComCtrls.TTrackBar, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Changed; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
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

function TComboBox.GetText: TCaption;
begin
  Result := inherited Text;
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

procedure TComboBox.SetText(const Value: TCaption);
var
  i: Integer;
begin
  inherited Text := Value;
  if Style = csDropDownList then
  begin
    i := Items.IndexOf(Value);
    if i > -1 then
    begin
      ItemIndex := i;
    end;
  end;
end;

{ TDateTimePicker }

procedure TDateTimePicker.Change;
begin
  inherited;
  case Kind of
    dtkDate:
      FOnPropertyChanged.Invoke(Self, 'Date', utPropertyChanged);
    dtkTime:
      FOnPropertyChanged.Invoke(Self, 'Time', utPropertyChanged);
  end;
end;

procedure TDateTimePicker.CMExit(var Message: TCMExit);
begin
  inherited;
  case Kind of
    dtkDate:
      FOnPropertyChanged.Invoke(Self, 'Date', utLostFocus);
    dtkTime:
      FOnPropertyChanged.Invoke(Self, 'Time', utLostFocus);
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

{ TForm }

procedure TForm.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

function TForm.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TFrame }

procedure TFrame.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

function TFrame.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TFrame.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  FOnPropertyChanged.Invoke(Self, 'BindingSource', utPropertyChanged);
end;

{ TGroupBox }

function TGroupBox.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TGroupBox.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  FOnPropertyChanged.Invoke(Self, 'BindingSource', utPropertyChanged);
end;

{ TListBox }

procedure TListBox.Click;
begin
  inherited;
  DoPropertyChanged('CurrentItem');
  DoPropertyChanged('ItemIndex');
end;

constructor TListBox.Create(AOwner: TComponent);
begin
  inherited;
  FOnCollectionChanged.Add(OnSourceCollectionChanged);
end;

procedure TListBox.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

function TListBox.GetCurrentItem: TObject;
begin
  if ItemIndex > -1 then
  begin
    Result := Items.Objects[ItemIndex];
  end
  else
  begin
    Result := nil;
  end;
end;

function TListBox.GetFilter: TPredicate<TObject>;
begin
  Result := FFilter;
end;

function TListBox.GetItemsSource: TList<TObject>;
begin
  Result := FItemsSource;
end;

function TListBox.GetItemTemplate: IDataTemplate;
begin
  if not Assigned(FItemTemplate) then
  begin
    FItemTemplate := TDefaultDataTemplate.Create();
  end;
  Result := FItemTemplate;
end;

function TListBox.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TListBox.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TListBox.OnSourceCollectionChanged(Sender: TObject;
  Item: TObject; Action: TCollectionNotification);
var
  i: Integer;
begin
  case Action of
    cnAdded:
    begin
      if not Assigned(FFilter) or FFilter(Item) then
      begin
        Items.AddObject(ItemTemplate.GetText(Item, 0), Item);
      end;
    end;
    cnRemoved:
    begin
      i := Items.IndexOfObject(Item);
      if i = ItemIndex then
      begin
        Items.Delete(i);
        DoPropertyChanged('CurrentItem');
      end
      else
      begin
        Items.Delete(i);
      end;
    end;
  end;
end;

procedure TListBox.SetCurrentItem(const Value: TObject);
begin
  // not implemented yet
end;

procedure TListBox.SetFilter(const Value: TPredicate<TObject>);
begin
  FFilter := Value;

  UpdateItems(True);
end;

procedure TListBox.SetItemsSource(const Value: TList<TObject>);
begin
  if FItemsSource <> Value then
  begin
    FItemsSource := Value;

    UpdateItems(True);
  end;
end;

procedure TListBox.SetItemTemplate(const Value: IDataTemplate);
begin
  if FItemTemplate <> Value then
  begin
    FItemTemplate := Value;

    UpdateItems(False);
  end;
end;

procedure TListBox.UpdateItems(AClearItems: Boolean);
var
  LItem: TObject;
begin
  if AClearItems then
  begin
    Items.Clear;
  end;

  if Assigned(FItemsSource) then
  begin
    for LItem in FItemsSource do
    begin
      if not Assigned(FFilter) or FFilter(LItem) then
      begin
        if AClearItems then
        begin
          Items.AddObject(ItemTemplate.GetText(LItem, 0), LItem);
        end
        else
        begin
          Items[Items.IndexOfObject(LItem)] := ItemTemplate.GetText(LItem, 0);
        end;
      end;
    end;
  end;
end;

{ TMemo }

procedure TMemo.Change;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utPropertyChanged);
end;

procedure TMemo.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utLostFocus);
end;

function TMemo.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
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

{ TPanel }

function TPanel.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TPanel.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  FOnPropertyChanged.Invoke(Self, 'BindingSource', utPropertyChanged);
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
