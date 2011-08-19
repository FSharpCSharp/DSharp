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

unit DSharp.Bindings.VCLControls;

interface

uses
  Classes,
  ComCtrls,
  CommCtrl,
  Controls,
  DSharp.Bindings,
  DSharp.Bindings.Collections,
  DSharp.Bindings.CollectionView.Adapters,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Core.DataTemplates.Default,
  DSharp.Core.Events,
  ExtCtrls,
  Forms,
  Messages,
  StdCtrls,
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

  TComboBox = class(StdCtrls.TComboBox, INotifyPropertyChanged, ICollectionView)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    FView: TCollectionViewStringsAdapter;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
  protected
    procedure Change; override;
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure Select; override;
    procedure SetItemIndex(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
    property View: TCollectionViewStringsAdapter read FView implements ICollectionView;
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

  TLabeledEdit = class(ExtCtrls.TLabeledEdit, INotifyPropertyChanged)
  private
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure Change; override;
  public
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TListBox = class(StdCtrls.TListBox, INotifyPropertyChanged, ICollectionView)
  private
    FView: TCollectionViewStringsAdapter;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure SetItemIndex(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
    property View: TCollectionViewStringsAdapter read FView implements ICollectionView;
  end;

  TListView = class(ComCtrls.TListView, INotifyPropertyChanged, ICollectionView)
  private
    FFilter: TPredicate<TObject>;
    FItemsSource: TList<TObject>;
    FItemTemplate: IDataTemplate;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CNNotify(var Message: TWMNotifyLV); message CN_NOTIFY;
    procedure DoCurrentItemPropertyChanged(Sender: TObject;
      PropertyName: string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
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
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure Edit(const Item: TLVItem); override;
    procedure OnSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction);
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

  TTreeView = class(ComCtrls.TTreeView, INotifyPropertyChanged, ICollectionView)
  private
    FFilter: TPredicate<TObject>;
    FItemsSource: TList<TObject>;
    FItemTemplate: IDataTemplate;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure CNNotify(var Message: TWMNotifyTV); message CN_NOTIFY;
    procedure DoCurrentItemPropertyChanged(Sender: TObject;
      PropertyName: string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
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
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure OnSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction);
  public
    constructor Create(AOwner: TComponent); override;
    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property Filter: TPredicate<TObject> read GetFilter write SetFilter;
    property ItemsSource: TList<TObject> read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent> read FOnCollectionChanged;
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
  DoPropertyChanged('Text');
end;

procedure TComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  DoPropertyChanged('ItemIndex', utLostFocus);
  DoPropertyChanged('Text', utLostFocus);
end;

constructor TComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FView := TCollectionViewStringsAdapter.Create(Self, Items);
end;

destructor TComboBox.Destroy;
begin
  FView.Free();
  inherited;
end;

procedure TComboBox.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
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
  DoPropertyChanged('ItemIndex');
end;

procedure TComboBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  DoPropertyChanged('ItemIndex');
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

{ TLabeledEdit }

procedure TLabeledEdit.Change;
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utPropertyChanged);
end;

procedure TLabeledEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  FOnPropertyChanged.Invoke(Self, 'Text', utLostFocus);
end;

function TLabeledEdit.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

{ TListBox }

constructor TListBox.Create(AOwner: TComponent);
begin
  inherited;
  FView := TCollectionViewStringsAdapter.Create(Self, Items);
end;

destructor TListBox.Destroy;
begin
  FView.Free();
  inherited;
end;

procedure TListBox.CMChanged(var Message: TCMChanged);
var
  LItem: TObject;
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  inherited;

  // multiselect not supported yet
  if FView.ItemIndex <> ItemIndex then
  begin
    LItem := FView.CurrentItem;
    begin
      if Supports(LItem, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Remove(FView.DoCurrentItemPropertyChanged);
      end;
    end;

    FView.ItemIndex := ItemIndex;
    LItem := FView.CurrentItem;
    begin
      if Supports(LItem, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Add(FView.DoCurrentItemPropertyChanged);
      end;
    end;

    DoPropertyChanged('View');
  end;
end;

procedure TListBox.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

function TListBox.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TListBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  Changed;
end;

{ TListView }

procedure TListView.CNNotify(var Message: TWMNotifyLV);
var
  LListItem: TListItem;
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  inherited;
  case Message.NMHdr.code of
    LVN_ITEMCHANGED:
    begin
      if {$IF COMPILERVERSION > 21}not Reading and{$IFEND} (Message.NMListView.uChanged = LVIF_STATE) then
      begin
        if (Message.NMListView.uOldState and LVIS_SELECTED <> 0)
          and (Message.NMListView.uNewState and LVIS_SELECTED = 0) then
        begin
          LListItem := Items[Message.NMListView.iItem];
          begin
            if Supports(LListItem.Data, INotifyPropertyChanged, LNotifyPropertyChanged) then
            begin
              LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
              LPropertyChanged.Remove(DoCurrentItemPropertyChanged);
            end;
          end;

          DoPropertyChanged('CurrentItem');
          DoPropertyChanged('ItemIndex');
        end else
        if (Message.NMListView.uOldState and LVIS_SELECTED = 0)
          and (Message.NMListView.uNewState and LVIS_SELECTED <> 0) then
        begin
          LListItem := Items[Message.NMListView.iItem];
          begin
            if Supports(LListItem.Data, INotifyPropertyChanged, LNotifyPropertyChanged) then
            begin
              LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
              LPropertyChanged.Add(DoCurrentItemPropertyChanged);
            end;
          end;

          DoPropertyChanged('CurrentItem');
          DoPropertyChanged('ItemIndex');
        end;
      end;
    end;
  end;
end;

constructor TListView.Create(AOwner: TComponent);
begin
  inherited;
  FOnCollectionChanged.Add(OnSourceCollectionChanged);
end;

procedure TListView.DoCurrentItemPropertyChanged(Sender: TObject;
  PropertyName: string; UpdateTrigger: TUpdateTrigger);
var
  i: Integer;
  LIndex: Integer;
  LListItem: TListItem;
begin
  LIndex := ItemIndex;
  if (LIndex > -1) and Assigned(FItemTemplate) then
  begin
    LListItem := Items[LIndex];
    for i := 0 to Pred(Columns.Count) do
    begin
      if i = 0 then
      begin
        LListItem.Caption := ItemTemplate.GetText(LListItem.Data, i);
      end
      else
      begin
        LListItem.SubItems[i - 1] := ItemTemplate.GetText(LListItem.Data, i);
      end;
    end;
  end;
end;

procedure TListView.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

procedure TListView.Edit(const Item: TLVItem);
begin
  inherited;
  // editing not supported yet
//  ItemTemplate.SetText(CurrentItem, 0, Selected.Caption);
end;

function TListView.GetCurrentItem: TObject;
begin
  if ItemIndex > -1 then
  begin
    Result := Items[ItemIndex].Data;
  end
  else
  begin
    Result := nil;
  end;
end;

function TListView.GetFilter: TPredicate<TObject>;
begin
  Result := FFilter;
end;

function TListView.GetItemsSource: TList<TObject>;
begin
  Result := FItemsSource;
end;

function TListView.GetItemTemplate: IDataTemplate;
begin
  if not Assigned(FItemTemplate) then
  begin
    FItemTemplate := TDefaultDataTemplate.Create();
  end;
  Result := FItemTemplate;
end;

function TListView.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TListView.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TListView.OnSourceCollectionChanged(Sender, Item: TObject;
  Action: TCollectionChangedAction);
var
  i: Integer;
begin
  // not fully implemented yet
  case Action of
    caAdd:
    begin
      if not Assigned(FFilter) or FFilter(Item) then
      begin
        Items.Add.Data := Item;
      end;
    end;
    caRemove:
    begin
      for i := 0 to Pred(Items.Count) do
      begin
        if Items[i].Data = Item then
        begin
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
  end;
end;

procedure TListView.SetCurrentItem(const Value: TObject);
begin
  // not implemented yet
end;

procedure TListView.SetFilter(const Value: TPredicate<TObject>);
begin
  FFilter := Value;

  UpdateItems(True);
end;

procedure TListView.SetItemsSource(const Value: TList<TObject>);
begin
  if FItemsSource <> Value then
  begin
    FItemsSource := Value;

    UpdateItems(True);
  end;
end;

procedure TListView.SetItemTemplate(const Value: IDataTemplate);
begin
  if FItemTemplate <> Value then
  begin
    FItemTemplate := Value;

    UpdateItems(False);
  end;
end;

procedure TListView.UpdateItems(AClearItems: Boolean);
var
  i: Integer;
  LItem: TObject;
  LListItem: TListItem;
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
        LListItem := nil;
        if AClearItems then
        begin
          LListItem := Items.Add;
          LListItem.Data := LItem;
        end
        else
        begin
          for i := 0 to Pred(Items.Count) do
          begin
            if Items[i].Data = LItem then
            begin
              LListItem := Items[i];
              LListItem.SubItems.Clear;
              Break;
            end;
          end;
        end;
        if Assigned(LListItem) then
        begin
          for i := 0 to Pred(Columns.Count) do
          begin
            if i = 0 then
            begin
              LListItem.Caption := ItemTemplate.GetText(LListItem.Data, i);
            end
            else
            begin
              LListItem.SubItems.Add(ItemTemplate.GetText(LListItem.Data, i));
            end;
          end;
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

{ TTreeView }

procedure TTreeView.CNNotify(var Message: TWMNotifyTV);
var
  LTreeNode: TTreeNode;
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  inherited;
  case Message.NMHdr.code of
    TVN_SELCHANGEDA, TVN_SELCHANGEDW:
    begin
      LTreeNode := Items.GetNode(Message.NMTreeView.itemOld.hItem);
      if Assigned(LTreeNode)
        and Supports(LTreeNode.Data, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Remove(DoCurrentItemPropertyChanged);
      end;

      DoPropertyChanged('CurrentItem');
      DoPropertyChanged('Selected');

      LTreeNode := Items.GetNode(Message.NMTreeView.itemNew.hItem);
      if Assigned(LTreeNode)
        and Supports(LTreeNode.Data, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Add(DoCurrentItemPropertyChanged);
      end;
    end;
  end;
end;

constructor TTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FOnCollectionChanged.Add(OnSourceCollectionChanged);
end;

procedure TTreeView.DoCurrentItemPropertyChanged(Sender: TObject;
  PropertyName: string; UpdateTrigger: TUpdateTrigger);
begin

end;

procedure TTreeView.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

function TTreeView.GetCurrentItem: TObject;
begin
  if SelectionCount > 0 then
  begin
    Result := Selections[0].Data;
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeView.GetFilter: TPredicate<TObject>;
begin
  Result := FFilter;
end;

function TTreeView.GetItemsSource: TList<TObject>;
begin
  Result := FItemsSource;
end;

function TTreeView.GetItemTemplate: IDataTemplate;
begin
  if not Assigned(FItemTemplate) then
  begin
    FItemTemplate := TDefaultDataTemplate.Create();
  end;
  Result := FItemTemplate;
end;

function TTreeView.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TTreeView.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TTreeView.OnSourceCollectionChanged(Sender, Item: TObject;
  Action: TCollectionChangedAction);
var
  i: Integer;
begin
  // not fully implemented yet
  case Action of
    caAdd:
    begin
      if not Assigned(FFilter) or FFilter(Item) then
      begin
        Items.AddObject(CreateNode(), '', Item);
      end;
    end;
    caRemove:
    begin
      for i := 0 to Pred(Items.Count) do
      begin
        if Items[i].Data = Item then
        begin
          if Items[i].Selected then
          begin
            Items.Delete(Items[i]);
            DoPropertyChanged('CurrentItem');
          end
          else
          begin
            Items.Delete(Items[i]);
          end;
        end;
      end;
    end;
  end;
end;

procedure TTreeView.SetCurrentItem(const Value: TObject);
begin
  // not implemented yet
end;

procedure TTreeView.SetFilter(const Value: TPredicate<TObject>);
begin
  FFilter := Value;

  UpdateItems(True);
end;

procedure TTreeView.SetItemsSource(const Value: TList<TObject>);
begin
  if FItemsSource <> Value then
  begin
    FItemsSource := Value;

    UpdateItems(True);
  end;
end;

procedure TTreeView.SetItemTemplate(const Value: IDataTemplate);
begin
  if FItemTemplate <> Value then
  begin
    FItemTemplate := Value;

    UpdateItems(False);
  end;
end;

procedure TTreeView.UpdateItems(AClearItems: Boolean);
var
  i: Integer;
  LItem: TObject;
  LTreeNode: TTreeNode;

  procedure CreateNodes(ANode: TTreeNode; AItemTemplate: IDataTemplate);
  var
    LItem: TObject;
    LItemTemplate: IDataTemplate;
    LTreeNode: TTreeNode;
  begin
    if Assigned(AItemTemplate) and (AItemTemplate.GetItemCount(ANode.Data) > 0) then
    begin
      for LItem in AItemTemplate.GetItems(ANode.Data) do
      begin
        LItemTemplate := AItemTemplate.GetItemTemplate(LItem);
        LTreeNode := Items.AddChildObject(ANode, LItemTemplate.GetText(LItem, -1), LItem);
        CreateNodes(LTreeNode, LItemTemplate);
      end;
    end;
  end;

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
          Items.AddChildObject(nil, ItemTemplate.GetText(LItem, -1), LItem);
        end
        else
        begin
          for i := 0 to Pred(Items.Count) do
          begin
            if Items[i].Data = LItem then
            begin
              LTreeNode := Items[i];
              LTreeNode.Text := ItemTemplate.GetText(LItem, -1);
              LTreeNode.DeleteChildren;
              CreateNodes(LTreeNode, ItemTemplate);
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
