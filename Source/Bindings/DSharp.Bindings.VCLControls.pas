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
  DSharp.Bindings.Collections,
  DSharp.Bindings.CollectionView,
  DSharp.Bindings.Notifications,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Core.DataTemplates.Default,
  DSharp.Core.Events,
  ExtCtrls,
  Forms,
  Grids,
  Messages,
  StdCtrls,
  SysUtils;

type
  TButton = class(StdCtrls.TButton)
  public
    procedure Click; override;
  end;

  TCheckBox = class(StdCtrls.TCheckBox, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Click; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TColorBox = class(ExtCtrls.TColorBox, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TComboBox = class(StdCtrls.TComboBox, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure Select; override;
    procedure SetItemIndex(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  published
    property Text: TCaption read GetText write SetText;
  end;

  TDateTimePicker = class(ComCtrls.TDateTimePicker, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEdit = class(StdCtrls.TEdit, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TForm = class(Forms.TForm, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TFrame = class(Forms.TFrame, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
  public
    constructor Create(AOwner: TComponent); override;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
  end;

  TGroupBox = class(StdCtrls.TGroupBox, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  public
    constructor Create(AOwner: TComponent); override;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
  end;

  TLabel = class(StdCtrls.TLabel);

  TLabeledEdit = class(ExtCtrls.TLabeledEdit, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TListBox = class(StdCtrls.TListBox, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    procedure SetItemIndex(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  end;

  TListView = class(ComCtrls.TListView, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure CNNotify(var Message: TWMNotifyLV); message CN_NOTIFY;
    procedure Edit(const Item: TLVItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  end;

  TMemo = class(StdCtrls.TMemo, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Change; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMonthCalendar = class(ComCtrls.TMonthCalendar, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CNNotify(var Message: TWMNotifyMC); message CN_NOTIFY;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTabSheet = class;

  TPageControl = class(ComCtrls.TPageControl, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    function GetActivePage: TTabSheet;
    procedure SetActivePage(const Value: TTabSheet);
    function GetPage(Index: Integer): TTabSheet;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure SetTabIndex(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Pages[Index: Integer]: TTabSheet read GetPage;
    property View: TCollectionView read FView implements ICollectionView;
  published
    property ActivePage: TTabSheet read GetActivePage write SetActivePage;
  end;

  TPanel = class(ExtCtrls.TPanel, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
  end;

  TRadioButton = class(StdCtrls.TRadioButton, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure SetChecked(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TRadioGroup = class(ExtCtrls.TRadioGroup, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Click; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TScrollBox = class(Forms.TScrollBox, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure AutoScrollInView(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  end;

  TStringGrid = class(Grids.TStringGrid, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    function CanEditShow: Boolean; override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  end;

  TTabSheet = class(ComCtrls.TTabSheet, INotifyPropertyChanged)
  private
    FBindingSource: TObject;
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FModalResult: TModalResult;
    procedure DoClose(var Action: TCloseAction);
    procedure SetModalResult(const Value: TModalResult);
    function GetPageControl: TPageControl;
    procedure SetPageControl(const Value: TPageControl);
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
    procedure SetBindingSource(const Value: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;
    function CloseQuery: Boolean;
    property BindingSource: TObject read FBindingSource write SetBindingSource;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property PageControl: TPageControl read GetPageControl write SetPageControl;
  published
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

  TTrackBar = class(ComCtrls.TTrackBar, INotifyPropertyChanged)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure Changed; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTreeView = class(ComCtrls.TTreeView, INotifyPropertyChanged, ICollectionView)
  private
    FNotifyPropertyChanged: INotifyPropertyChanged;
    FView: TCollectionView;
    property NotifyPropertyChanged: INotifyPropertyChanged
      read FNotifyPropertyChanged implements INotifyPropertyChanged;
  protected
    procedure CNNotify(var Message: TWMNotifyTV); message CN_NOTIFY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TCollectionView read FView implements ICollectionView;
  end;

implementation

uses
  DSharp.Bindings.CollectionView.Adapters,
  DSharp.Bindings.CollectionView.VCLAdapters,
  DSharp.Bindings.Exceptions,
  DSharp.Core.Reflection;

{ TButton }

procedure TButton.Click;
var
  Control: TWinControl;
begin
  inherited;

  Control := Parent;
  while Assigned(Control) do
  begin
    if Control is TTabSheet then
    begin
      TTabSheet(Control).ModalResult := ModalResult;
      Break;
    end;
    Control := Control.Parent;
  end;
end;

{ TCheckBox }

constructor TCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TCheckBox.Click;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Checked');
  NotifyPropertyChanged.DoPropertyChanged('State');
end;

procedure TCheckBox.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Checked', utLostFocus);
    NotifyPropertyChanged.DoPropertyChanged('State', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

{ TColorBox }

constructor TColorBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TColorBox.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Selected')
end;

procedure TColorBox.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Selected', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

{ TComboBox }

constructor TComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewStringsAdapter.Create(Self, Items);
end;

destructor TComboBox.Destroy;
begin
  FView.Free();
  inherited;
end;

procedure TComboBox.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

procedure TComboBox.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('ItemIndex', utLostFocus);
    NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

function TComboBox.GetText: TCaption;
begin
  Result := inherited Text;
end;

procedure TComboBox.Select;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('ItemIndex');
end;

procedure TComboBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('ItemIndex');
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

constructor TDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TDateTimePicker.Change;
begin
  inherited;
  case Kind of
    dtkDate:
      NotifyPropertyChanged.DoPropertyChanged('Date');
    dtkTime:
      NotifyPropertyChanged.DoPropertyChanged('Time');
  end;
end;

procedure TDateTimePicker.CMExit(var Message: TCMExit);
begin
  try
    case Kind of
      dtkDate:
        NotifyPropertyChanged.DoPropertyChanged('Date', utLostFocus);
      dtkTime:
        NotifyPropertyChanged.DoPropertyChanged('Time', utLostFocus);
    end;
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

procedure TDateTimePicker.WMLButtonDown(var Message: TWMLButtonDown);
begin
  SetFocus;
  if not Focused then
  begin
    Perform(WM_KILLFOCUS, 0, 0);
  end
  else
  begin
    inherited;
  end;
end;

{ TEdit }

constructor TEdit.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TEdit.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

procedure TEdit.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

{ TForm }

constructor TForm.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TForm.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  NotifyPropertyChanged.DoPropertyChanged(APropertyName, AUpdateTrigger);
end;

{ TFrame }

constructor TFrame.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TFrame.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  NotifyPropertyChanged.DoPropertyChanged(APropertyName, AUpdateTrigger);
end;

procedure TFrame.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

{ TGroupBox }

constructor TGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TGroupBox.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

{ TLabeledEdit }

constructor TLabeledEdit.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TLabeledEdit.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

procedure TLabeledEdit.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

{ TListBox }

constructor TListBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewStringsAdapter.Create(Self, Items);
end;

destructor TListBox.Destroy;
begin
  FView.Free();
  inherited;
end;

procedure TListBox.CMChanged(var Message: TCMChanged);
begin
  inherited;

  // multiselect not supported yet
  if FView.ItemIndex <> ItemIndex then
  begin
    FView.ItemIndex := ItemIndex;

    NotifyPropertyChanged.DoPropertyChanged('View');
  end;
end;

procedure TListBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  Changed;
end;

{ TListView }

constructor TListView.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewListItemsAdapter.Create(Self, Items, Columns);
end;

destructor TListView.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TListView.CNNotify(var Message: TWMNotifyLV);
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
          NotifyPropertyChanged.DoPropertyChanged('View');
        end else
        if (Message.NMListView.uOldState and LVIS_SELECTED = 0)
          and (Message.NMListView.uNewState and LVIS_SELECTED <> 0) then
        begin
          if FView.ItemIndex <> Message.NMListView.iItem then
          begin
            FView.ItemIndex := Message.NMListView.iItem;
          end;

          NotifyPropertyChanged.DoPropertyChanged('View');
        end;
      end;
    end;
  end;
end;

procedure TListView.Edit(const Item: TLVItem);
begin
  inherited;
  FView.ItemTemplate.SetText(FView.CurrentItem, 0, Selected.Caption);
end;

{ TMemo }

constructor TMemo.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TMemo.Change;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Text');
end;

procedure TMemo.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Text', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;

end;

{ TMonthCalendar }

constructor TMonthCalendar.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TMonthCalendar.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Date', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

procedure TMonthCalendar.CNNotify(var Message: TWMNotifyMC);
begin
  inherited;
  case Message.NMHdr.code of
    MCN_SELCHANGE:
      NotifyPropertyChanged.DoPropertyChanged('Date');
  end;
end;

{ TPageControl }

constructor TPageControl.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewPageControlAdapter.Create(Self, Self);
end;

destructor TPageControl.Destroy;
begin
  FView.Free();
  inherited;
end;

function TPageControl.GetActivePage: TTabSheet;
begin
  Result := inherited ActivePage as TTabSheet;
end;

function TPageControl.GetPage(Index: Integer): TTabSheet;
begin
  Result := inherited Pages[Index] as TTabSheet;
end;

procedure TPageControl.SetActivePage(const Value: TTabSheet);
begin
  inherited ActivePage := Value;
end;

procedure TPageControl.SetTabIndex(Value: Integer);
begin
  inherited;
  if (FView.ItemIndex <> Value) and (Value > -1) then
  begin
    FView.ItemIndex := Value;
  end;
  NotifyPropertyChanged.DoPropertyChanged('ActivePage');
  NotifyPropertyChanged.DoPropertyChanged('ActivePageIndex');
  NotifyPropertyChanged.DoPropertyChanged('TabIndex');
end;

{ TPanel }

constructor TPanel.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TPanel.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

{ TRadioButton }

constructor TRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TRadioButton.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Checked', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

procedure TRadioButton.SetChecked(Value: Boolean);
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Checked');
end;

{ TRadioGroup }

constructor TRadioGroup.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TRadioGroup.Click;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('ItemIndex');
end;

procedure TRadioGroup.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('ItemIndex', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

{ TScrollBox }

constructor TScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewAdapter.Create(Self);
end;

destructor TScrollBox.Destroy;
begin
  FView.Free();
  inherited;
end;

procedure TScrollBox.AutoScrollInView(AControl: TControl);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Pred(ControlCount) do
  begin
    if AControl.Owner = Controls[i] then
    begin
      FView.ItemIndex := FView.ItemsSource.IndexOf(TObject(AControl.Owner.Tag));
      Break;
    end;
  end;

  NotifyPropertyChanged.DoPropertyChanged('View');
end;

{ TStringGrid }

constructor TStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewStringGridAdapter.Create(Self, Self);
end;

destructor TStringGrid.Destroy;
begin
  FView.Free();
  inherited;
end;

function TStringGrid.CanEditShow: Boolean;
begin
  Result := inherited and (FView.CurrentItem <> nil);
end;

function TStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited;
  FView.ItemIndex := ARow - FixedRows;
  NotifyPropertyChanged.DoPropertyChanged('Selected');
end;

procedure TStringGrid.SetEditText(ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FView.BeginUpdate;
  try
    if (FView.ItemsSource <> nil) and (FView.ItemTemplate <> nil)
      and (FView.ItemsSource.Count > ARow - FixedRows) then
    begin
      FView.ItemTemplate.SetText(FView.ItemsSource[ARow - FixedRows].ToObject, ACol - FixedCols, Value);
    end;
  finally
    FView.EndUpdate;
  end;
end;

{ TTabSheet }

constructor TTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TTabSheet.Close;
var
  CloseAction: TCloseAction;
begin
  if CloseQuery then
  begin
    CloseAction := caFree;
    DoClose(CloseAction);
    if CloseAction = caFree then
    begin
      Free;
    end;
  end;
end;

function TTabSheet.CloseQuery: Boolean;
begin
  Result := True;
  if Assigned(FOnCloseQuery) then
  begin
    FOnCloseQuery(Self, Result);
  end;
end;

destructor TTabSheet.Destroy;
begin
  if Assigned(PageControl) and Assigned(PageControl.View.ItemsSource) then
  begin
    PageControl.View.ItemsSource.Remove(TObject(Tag));
  end;
  inherited;
end;

procedure TTabSheet.DoClose(var Action: TCloseAction);
begin
  if Assigned(FOnClose) then
  begin
    FOnClose(Self, Action);
  end;
end;

function TTabSheet.GetPageControl: TPageControl;
begin
  Result := inherited PageControl as TPageControl;
end;

procedure TTabSheet.SetBindingSource(const Value: TObject);
begin
  FBindingSource := Value;
  NotifyPropertyChanged.DoPropertyChanged('BindingSource');
end;

procedure TTabSheet.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
  if FModalResult <> mrNone then
  begin
    Close;
  end;
end;

procedure TTabSheet.SetPageControl(const Value: TPageControl);
begin
  inherited PageControl := Value;
end;

{ TTrackBar }

constructor TTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
end;

procedure TTrackBar.Changed;
begin
  inherited;
  NotifyPropertyChanged.DoPropertyChanged('Position');
end;

procedure TTrackBar.CMExit(var Message: TCMExit);
begin
  try
    NotifyPropertyChanged.DoPropertyChanged('Position', utLostFocus);
    inherited;
  except
    on EValidationError do
    begin
      SetFocus;
    end;
  end;
end;

{ TTreeView }

constructor TTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewTreeNodesAdapter.Create(Self, Items);
end;

procedure TTreeView.CNNotify(var Message: TWMNotifyTV);
begin
  inherited;
  case Message.NMHdr.code of
    TVN_SELCHANGEDA, TVN_SELCHANGEDW:
    begin
      FView.ItemIndex := NativeInt(Items.GetNode(Message.NMTreeView.itemNew.hItem));
      NotifyPropertyChanged.DoPropertyChanged('Selected');
    end;
  end;
end;

destructor TTreeView.Destroy;
begin
  FView.Free();
  inherited;
end;

end.
