(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Windows.TreeViewPresenter;

interface

uses
  ActiveX,
  Classes,
  ComCtrls,
  Controls,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Core.Events,
  DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.CustomPresenter,
  Menus,
  SysUtils,
  Types,
  VirtualTrees;

type
  TDragOperation = VirtualTrees.TDragOperation;
  TDropMode = VirtualTrees.TDropMode;

  TCompareEvent = procedure(Sender: TObject; Item1, Item2: TObject;
    ColumnIndex: Integer; var Result: Integer) of object;
  TDragBeginEvent = procedure(Sender: TObject; var AllowDrag: Boolean) of object;
  TDragOverEvent = procedure(Sender: TObject; Source: TObject; TargetItem: TObject;
    var AllowDrop: Boolean) of object;
  TDragDropEvent = procedure(Sender: TObject; Source: TObject; TargetItem: TObject;
    DragOperation: TDragOperation; var DropMode: TDropMode;
    var Handled: Boolean) of object;

  TCheckSupport = (csNone, csSimple, csTriState, csRadio);
  TSelectionMode = (smSingle, smLevel, smMulti, smNone);

  TTreeViewPresenter = class(TCustomPresenter)
  private
    FAllowClearSelection: Boolean;
    FAllowMove: Boolean;
    FCheckedItems: IList<TObject>;
    FCheckSupport: TCheckSupport;
    FCollectionChanging: Integer;
    FCurrentNode: PVirtualNode;
    FExpandedItems: IList<TObject>;
    FHitInfo: THitInfo;
    FListMode: Boolean;
    FOnCompare: TCompareEvent;
    FOnDragBegin: TDragBeginEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnKeyAction: TKeyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FProgressBar: TProgressBar;
    FSelectedItems: IList<TObject>;
    FShowHeader: Boolean;
    FSelectionMode: TSelectionMode;
    FSorting: Boolean;
    FSyncing: Boolean;
    FSyncMode: Boolean;
    FTreeView: TVirtualStringTree;

    procedure DoAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure DoBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure DoChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DoDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DoDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure DoDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure DoEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DoEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure DoExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoFilterNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure DoGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure DoGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DoHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure DoIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);
    procedure DoInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure DoNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

    procedure DrawCheckBox(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Boolean);
    procedure DrawProgressBar(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Integer);
    procedure DrawImage(TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect; Value: Integer);

    procedure ExpandNode(Node: PVirtualNode);
    function GetCheckedItem: TObject;
    function GetCheckedItems: IList<TObject>;
    function GetExpandedItems: IList<TObject>;
    procedure GetItemNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure GetItemsNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode): IList;
    function GetNodeItemsAsObject(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetParentItem(const Level: Integer): TObject;
    function GetSelectedItem: TObject;
    function GetSelectedItems: IList<TObject>;

    function CalcCheckBoxRect(const Rect: TRect): TRect;
    function CalcImageRect(const Rect: TRect): TRect;
    function IsMouseInCheckBox(Node: PVirtualNode; Column: TColumnIndex): Boolean;
    function IsMouseInToggleIcon(HitInfo: THitInfo): Boolean;
    procedure ToggleIcon(Node: PVirtualNode; Column: TColumnIndex);

    procedure ReadMultiSelect(Reader: TReader);
    procedure ResetRootNodeCount;

    procedure SetCheckedItem(const Value: TObject);
    procedure SetCheckedItems(const Value: IList<TObject>);
    procedure SetCheckSupport(const Value: TCheckSupport);
    procedure SetExpandedItems(const Value: IList<TObject>);
    procedure SetListMode(const Value: Boolean);
    procedure SetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode; Item: TObject);
    procedure SetNodeItems(Tree: TBaseVirtualTree; Node: PVirtualNode; Items: IList);
    procedure SetSelectedItem(const Value: TObject);
    procedure SetSelectedItems(const Value: IList<TObject>);
    procedure SetSelectionMode(const Value: TSelectionMode);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetSorting(const Value: Boolean);
    procedure SetTreeView(const Value: TVirtualStringTree);

    procedure UpdateCheckedItems;
    procedure UpdateExpandedItems;
    procedure UpdateSelectedItems;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoCheckedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoDblClick(Sender: TObject); override;
    procedure DoExpandedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoSelectedItemsChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction);
    procedure DoSourceCollectionChanged(Sender: TObject; const Item: TObject;
      Action: TCollectionChangedAction); override;
    function GetCurrentItem: TObject; override;
    procedure SetCurrentItem(const Value: TObject); override;
    procedure InitColumns; override;
    procedure InitControl; override;
    procedure InitEvents; override;
    procedure InitProperties; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyFilter; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure DeleteItems(Items: IList<TObject>); deprecated 'use View.ItemsSource.RemoveRange instead';
    procedure FullCollapse;
    procedure FullExpand;

    procedure Refresh; override;

    property CheckedItem: TObject read GetCheckedItem write SetCheckedItem;
    property CheckedItems: IList<TObject> read GetCheckedItems write SetCheckedItems;
    property ExpandedItems: IList<TObject> read GetExpandedItems write SetExpandedItems;
    property ParentItem[const Level: Integer]: TObject read GetParentItem;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property SelectedItems: IList<TObject> read GetSelectedItems write SetSelectedItems;
  published
    property AllowClearSelection: Boolean
      read FAllowClearSelection write FAllowClearSelection default True;
    property AllowMove: Boolean read FAllowMove write FAllowMove default True;
    property CheckSupport: TCheckSupport read FCheckSupport write SetCheckSupport default csNone;
    property ListMode: Boolean read FListMode write SetListMode default False;
    property OnCompare: TCompareEvent read FOnCompare write FOnCompare;
    property OnDragBegin: TDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnKeyAction: TKeyEvent read FOnKeyAction write FOnKeyAction;
    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property SelectionMode: TSelectionMode read FSelectionMode write SetSelectionMode default smSingle;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property Sorting: Boolean read FSorting write SetSorting default True;
    property SyncMode: Boolean read FSyncMode write FSyncMode default False;
    property TreeView: TVirtualStringTree read FTreeView write SetTreeView;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Windows.ColumnDefinitions.ControlTemplate,
  DSharp.Windows.ControlTemplates,
  Math,
  Rtti,
  Themes,
  TypInfo,
  Windows;

const
  CDefaultCellRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Item: TObject;
    Items: IList;
    ItemsAsObject: TObject;
  end;

var
  CheckBoxSize: Byte;

procedure Synchronize(Target, Source: IList<TObject>);
var
  i: Integer;
begin
  i := 0;
  while i < Target.Count do
  begin
    if Source.Contains(Target[i]) then
    begin
      Inc(i);
    end
    else
    begin
      Target.Delete(i);
    end;
  end;
  for i := 0 to Pred(Source.Count) do
  begin
    if not Target.Contains(Source[i]) then
    begin
      Target.Add(Source[i]);
    end;
  end;
end;

{$IF CompilerVersion < 23}
type
  TThemeServicesHelper = class helper for TThemeServices
    function Enabled: Boolean;
  end;

function TThemeServicesHelper.Enabled: Boolean;
begin
  Result := ThemesEnabled;
end;

function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}

{ TTreeViewPresenter }

constructor TTreeViewPresenter.Create(AOwner: TComponent);
begin
  FCheckedItems := TList<TObject>.Create();
  FCheckedItems.OnCollectionChanged.Add(DoCheckedItemsChanged);
  FExpandedItems := TList<TObject>.Create();
  FExpandedItems.OnCollectionChanged.Add(DoExpandedItemsChanged);
  FSelectedItems := TList<TObject>.Create();
  FSelectedItems.OnCollectionChanged.Add(DoSelectedItemsChanged);
  inherited;
  FAllowClearSelection := True;
  FAllowMove := True;
  FShowHeader := True;
  FSorting := True;
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Smooth := True;
  FProgressBar.Visible := False;
end;

destructor TTreeViewPresenter.Destroy;
begin
  FCheckedItems.OnCollectionChanged.Remove(DoCheckedItemsChanged);
  FExpandedItems.OnCollectionChanged.Remove(DoExpandedItemsChanged);
  FSelectedItems.OnCollectionChanged.Remove(DoSelectedItemsChanged);
  inherited;
end;

procedure TTreeViewPresenter.ApplyFilter;
var
  LNode: PVirtualNode;
begin
  LNode := FTreeView.GetFirst();
  while Assigned(LNode) do
  begin
    DoFilterNode(FTreeView, LNode);
    LNode := FTreeView.GetNext(LNode);
  end;
end;

procedure TTreeViewPresenter.BeginUpdate;
begin
  inherited;
  FTreeView.BeginUpdate();
end;

function TTreeViewPresenter.CalcCheckBoxRect(const Rect: TRect): TRect;
begin
  Result.Left := Rect.Left + (RectWidth(Rect) - CheckBoxSize) div 2;
  Result.Top := Rect.Top + (RectHeight(Rect) - CheckBoxSize) div 2;
  Result.Right := Result.Left + CheckBoxSize;
  Result.Bottom := Result.Top + CheckBoxSize;
end;

function TTreeViewPresenter.CalcImageRect(const Rect: TRect): TRect;
begin
  Result.Left := Rect.Left + (RectWidth(Rect) - ImageList.Width) div 2;
  Result.Top := Rect.Top + (RectHeight(Rect) - ImageList.Height) div 2;
  Result.Right := Result.Left + ImageList.Width;
  Result.Bottom := Result.Top + ImageList.Height;
end;

procedure TTreeViewPresenter.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('MultiSelect', ReadMultiSelect, nil, False);
end;

procedure TTreeViewPresenter.DeleteItems(Items: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(Items) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and Items.Contains(LItem) then
      begin
        FTreeView.DeleteNode(LNode);
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  LItem: TObject;
  LDataTemplate: IDataTemplate;
  LControlTemplate: IControlTemplate;
  LValue: TValue;
begin
  LItem := GetNodeItem(Sender, Node);
  LDataTemplate := GetItemTemplate(LItem);
  if Supports(LDataTemplate, IControlTemplate, LControlTemplate) then
  begin
    LControlTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, ImageList,
      dmAfterCellPaint, Sender.Selected[Node]);
  end;

  if Assigned(ColumnDefinitions) and (Column > -1) then
  begin
    LValue := LDataTemplate.GetValue(LItem, Column);
    case ColumnDefinitions[Column].ColumnType of
      TColumnType.ctCheckBox:
        DrawCheckBox(TargetCanvas, Node, Column, CellRect, LValue.AsBoolean);
      TColumnType.ctProgressBar:
        DrawProgressBar(TargetCanvas, Node, Column, CellRect, LValue.AsOrdinal);
      TColumnType.ctImage:
        DrawImage(TargetCanvas, Node, Column, CellRect, LValue.AsOrdinal +
          ColumnDefinitions[Column].ImageIndexOffset);
    end;
  end;
end;

procedure TTreeViewPresenter.DoBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LItem: TObject;
  LItemTemplate: IControlTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, ImageList,
      dmBeforeCellPaint, Sender.Selected[Node]);
  end;
end;

procedure TTreeViewPresenter.DoChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if FSelectionMode <> smNone then
  begin
    UpdateSelectedItems();

    DoPropertyChanged('View');
    DoPropertyChanged('SelectedItem');
    DoPropertyChanged('SelectedItems');

    if Assigned(FOnSelectionChanged) then
    begin
      FOnSelectionChanged(Self);
    end;
  end;
end;

procedure TTreeViewPresenter.DoChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateCheckedItems();

  DoPropertyChanged('CheckedItem');
  DoPropertyChanged('CheckedItems');
end;

procedure TTreeViewPresenter.DoCheckedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState) then
  begin
    if FCollectionChanging = 0 then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        if GetNodeItem(FTreeView, LNode) = Item then
        begin
          case Action of
            caAdd: FTreeView.CheckState[LNode] := csCheckedNormal;
            caRemove: FTreeView.CheckState[LNode] := csUncheckedNormal;
          end;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;

    if FSyncMode and not FSyncing then
    try
      FSyncing := True;
      SetSelectedItems(FCheckedItems);
    finally
      FSyncing := False;
    end;
  end;
end;

procedure TTreeViewPresenter.DoCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateExpandedItems();

  DoPropertyChanged('ExpandedItems');
end;

procedure TTreeViewPresenter.DoCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  LItem1, LItem2: TObject;
begin
  if Column > -1 then
  begin
    LItem1 := GetNodeItem(Sender, Node1);
    LItem2 := GetNodeItem(Sender, Node2);

    if Assigned(FOnCompare) then
    begin
      FOnCompare(Self, LItem1, LItem2, Column, Result);
    end
    else
    begin
      Result := View.ItemTemplate.CompareItems(LItem1, LItem2, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoDblClick(Sender: TObject);
var
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
begin
  LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
  FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, False, LHitInfo);

  if not IsMouseInToggleIcon(LHitInfo) then
  begin
    if FListMode and Assigned(LHitInfo.HitNode)
      and (LHitInfo.HitColumn < 1)
      and ((hiOnNormalIcon in LHitInfo.HitPositions)
      or (not Assigned(OnDoubleClick) and not Assigned(Action))) then
    begin
      FTreeView.ToggleNode(LHitInfo.HitNode);
    end
    else
    begin
      if ([hiOnItemButton..hiOnItemCheckbox] * LHitInfo.HitPositions = [])
        and Assigned(LHitInfo.HitNode)
        and not IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      begin
        inherited;
      end;
    end;
  end
  else
  begin
    if Assigned(ColumnDefinitions) and (LHitInfo.HitColumn > -1)
      and (LHitInfo.HitColumn < ColumnDefinitions.Count)
      and (ColumnDefinitions[LHitInfo.HitColumn].ToggleMode = tmDoubleClick) then
    begin
      ToggleIcon(LHitInfo.HitNode, LHitInfo.HitColumn);
    end;
  end;
end;

procedure TTreeViewPresenter.DoDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := FAllowMove;

  if Assigned(FOnDragBegin) then
  begin
    FOnDragBegin(Self, Allowed);
  end;
end;

procedure TTreeViewPresenter.DoDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  i: Integer;
  LItem: TObject;
  LNode: PVirtualNode;
  LSelectedNodes: TNodeArray;
  LHandled: Boolean;
begin
  LNode := Sender.DropTargetNode;
  LItem := GetNodeItem(Sender, LNode);
  LHandled := False;

  LSelectedNodes := Sender.GetSortedSelection(False);
  if ssCtrl in Shift then
  begin
    if Assigned(FOnDragDrop) then
    begin
      FOnDragDrop(Sender, Source, LItem, doCopy, Mode, LHandled);
    end;
    Sender.ReinitNode(LNode, True);
  end
  else
  begin
    if Assigned(FOnDragDrop) then
    begin
      FOnDragDrop(Sender, Source, LItem, doMove, Mode, LHandled);
    end;

    if not LHandled and (Sender = Source) then
    begin
      Inc(FUpdateCount);
      try
        for i := Low(LSelectedNodes) to High(LSelectedNodes) do
        begin
          FCurrentNode := LSelectedNodes[i].Parent;
          case Mode of
            dmNowhere: FTreeView.MoveTo(LSelectedNodes[i], nil, amAddChildLast, False);
            dmAbove: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertBefore, False);
            dmOnNode:
            begin
              if FCurrentNode <> LNode then
              begin
                FTreeView.MoveTo(LSelectedNodes[i], LNode, amAddChildLast, False);
                ExpandNode(LNode);
              end;
            end;
            dmBelow: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertAfter, False);
          end;
        end;
      finally
        Dec(FUpdateCount);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  LItem: TObject;
  LNode: PVirtualNode;
  LItemNode: PVirtualNode;
begin
  if Pt.Y > -1 then
  begin
    LNode := Sender.GetNodeAt(Pt.X, Pt.Y);
    LItem := GetNodeItem(Sender, LNode);
    case Mode of
      dmAbove, dmBelow: Accept := FAllowMove;
    end;
    if Assigned(FOnDragOver) then
    begin
      FOnDragOver(Sender, Source, LItem, Accept);
    end
    else
    begin
      if Sender = Source then
      begin
        if not Assigned(LNode) then
        begin
          LNode := Sender.RootNode;
        end;
        Accept := Assigned(GetNodeItemsAsObject(Sender, LNode));
        if Accept then
        begin
          for LItemNode in Sender.SelectedNodes do
          begin
            if (LItemNode = LNode) or (LItemNode.Parent = LNode) or
              Sender.HasAsParent(LNode, LItemNode) then
            begin
              Accept := False;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if FTreeView.Header.SortColumn = Column then
  begin
    FTreeView.Sort(Node.Parent, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
  end;
end;

procedure TTreeViewPresenter.DoEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Assigned(ColumnDefinitions)
    and (Column > -1) and (ColumnDefinitions[Column].ColumnType = ctText)
    and ColumnDefinitions[Column].AllowEdit;
end;

procedure TTreeViewPresenter.DoExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateExpandedItems();

  DoPropertyChanged('ExpandedItems');
end;

procedure TTreeViewPresenter.DoExpandedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FCollectionChanging = 0) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      if GetNodeItem(FTreeView, LNode) = Item then
      begin
        case Action of
          caAdd: ExpandNode(LNode);
          caRemove: FTreeView.Expanded[LNode] := False;
        end;
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoFilterNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i: Integer;
  LItem: TObject;
  LAccepted: Boolean;
begin
  LItem := GetNodeItem(Sender, Node);
  LAccepted := True;

  View.Filter.Invoke(LItem, LAccepted);
  Sender.IsFiltered[Node] := not LAccepted;

  if Assigned(ColumnDefinitions) then
  begin
    for i := 0 to Pred(ColumnDefinitions.Count) do
    begin
      if Assigned(ColumnDefinitions[i].Filter) then
      begin
        Sender.IsFiltered[Node] := Sender.IsFiltered[Node]
          or not ColumnDefinitions[i].Filter(LItem);
      end;
    end;
  end;

  if Sender.IsFiltered[Node] and Sender.Selected[Node] then
  begin
    Sender.Selected[Node] := False;
  end;
end;

procedure TTreeViewPresenter.DoFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LItem: TObject;
begin
  LItem := GetNodeItem(Sender, Node);
  if Assigned(LItem) then
  begin
    // nothing to do here yet
  end;
end;

procedure TTreeViewPresenter.DoFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SetNodeItems(Sender, Node, nil);
end;

procedure TTreeViewPresenter.DoGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    HintText := LItemTemplate.GetHint(LItem, Column);
  end;
end;

procedure TTreeViewPresenter.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  if Kind in [ikNormal, ikSelected] then
  begin
    LItem := GetNodeItem(Sender, Node);
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      ImageIndex := LItemTemplate.GetImageIndex(LItem, Column);
    end;
  end;
end;

procedure TTreeViewPresenter.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    if Assigned(ColumnDefinitions) and (Column > -1)
      and (ColumnDefinitions[Column].ColumnType = TColumnType.ctText) then
    begin
      CellText := LItemTemplate.GetText(LItem, Column);
    end
    else
    begin
      CellText := '';
    end;
  end;
end;

procedure TTreeViewPresenter.DoHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if FSorting then
  begin
    if Sender.SortColumn <> HitInfo.Column then
    begin
      Sender.SortColumn := HitInfo.Column;
    end
    else
    begin
      if Sender.SortDirection = sdAscending then
      begin
        Sender.SortDirection := sdDescending;
      end
      else
      begin
        Sender.SortDirection := sdAscending;
      end;
    end;

    if Sender.SortColumn = -1 then
    begin
      Refresh();
    end;
  end;
end;

procedure TTreeViewPresenter.DoIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);
var
  LCellText: string;
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LCellText := LItemTemplate.GetText(LItem, ColumnDefinitions.MainColumnIndex);
  end
  else
  begin
    LCellText := '';
  end;

  Result := StrLIComp(PChar(SearchText), PChar(LCellText),
    Min(Length(SearchText), Length(LCellText)));
end;

procedure TTreeViewPresenter.DoInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LItem: TObject;
  LItems: IList;
  LItemTemplate: IDataTemplate;
  LParentItem: TObject;
  LParentItems: TObject;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  case FCheckSupport of
    csTriState: Node.CheckType := ctTriStateCheckBox;
    csRadio: Node.CheckType := ctRadioButton;
  else
    Node.CheckType := ctCheckBox;
  end;

  if Assigned(ParentNode) then
  begin
    LParentItems := GetNodeItemsAsObject(Sender, ParentNode);
    if Assigned(LParentItems) then
    begin
      // assume covariance to TListBase<TObject>
      LItem := TListBase<TObject>(LParentItems).Items[Node.Index];
    end
    else
    begin
      LParentItem := GetNodeItem(Sender, ParentNode);
      LItemTemplate := GetItemTemplate(LParentItem);
      LItem := LItemTemplate.GetItem(LParentItem, Node.Index);
    end;
  end
  else
  begin
    LItem := View.ItemTemplate.GetItem(View.ItemsSource.AsObject, Node.Index);
  end;

  SetNodeItem(Sender, Node, LItem);

  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItems := LItemTemplate.GetItems(LItem);
    SetNodeItems(Sender, Node, LItems);
    if Assigned(LItems) then
    begin
      Sender.ChildCount[Node] := LItems.Count;
    end
    else
    begin
      Sender.ChildCount[Node] := LItemTemplate.GetItemCount(LItem);
    end;
  end
  else
  begin
    Sender.ChildCount[Node] := 0;
  end;

  DoFilterNode(Sender, Node);
end;

procedure TTreeViewPresenter.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  LAllowed: Boolean;
  LNodes: TNodeArray;
begin
  if Key = VK_RETURN then
  begin
    if Assigned(Action) then
    begin
      if FTreeView.SelectedCount > 0 then
      begin
        Action.Execute();
      end;
    end;
  end;

  // moving elements with Ctrl+Up and Ctrl+Down only when sorting is off
  if FAllowMove and (ssCtrl in Shift) and (FTreeView.Header.SortColumn = -1) then
  begin
    case Key of
      VK_UP:
      begin
        LAllowed := True;
        LNodes := FTreeView.GetSortedSelection(False);
        for i := Low(LNodes) to High(LNodes) do
        begin
          if LNodes[i].PrevSibling = nil then
          begin
            LAllowed := False;
            Break;
          end;
        end;
        if LAllowed then
        begin
          Inc(FUpdateCount);
          try
            for i := Low(LNodes) to High(LNodes) do
            begin
              FCurrentNode := LNodes[i].Parent;
              FTreeView.MoveTo(LNodes[i], LNodes[i].PrevSibling, amInsertBefore, False);
            end;
          finally
            Dec(FUpdateCount);
          end;
        end;
      end;
      VK_DOWN:
      begin
        LAllowed := True;
        LNodes := FTreeView.GetSortedSelection(False);
        for i := High(LNodes) downto Low(LNodes) do
        begin
          if LNodes[i].NextSibling = nil then
          begin
            LAllowed := False;
            Break;
          end;
        end;
        if LAllowed then
        begin
          Inc(FUpdateCount);
          try
            for i := High(LNodes) downto Low(LNodes) do
            begin
              FCurrentNode := LNodes[i].Parent;
              FTreeView.MoveTo(LNodes[i], LNodes[i].NextSibling, amInsertAfter, False);
            end;
          finally
            Dec(FUpdateCount);
          end;
        end;
      end;
    end;
  end;

  if Assigned(FOnKeyAction) then
  begin
    FOnKeyAction(Self, Key, Shift);
  end;
end;

procedure TTreeViewPresenter.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LHitInfo: THitInfo;
begin
  if not (ssDouble in Shift) then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, False, LHitInfo);
    if not Assigned(LHitInfo.HitNode) then
    begin
      if FAllowClearSelection then
      begin
        FTreeView.ClearSelection();
      end
      else
      begin
        Abort;
      end;
    end
    else
    begin
      if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      begin
        FTreeView.RepaintNode(LHitInfo.HitNode);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  LHitInfo: THitInfo;
begin
  if GetAsyncKeyState(VK_LBUTTON) = 0 then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, True, LHitInfo);

    if Assigned(FHitInfo.HitNode) and (FHitInfo.HitNode <> LHitInfo.HitNode) then
    begin
      FTreeView.RepaintNode(FHitInfo.HitNode);
    end;

    if Assigned(LHitInfo.HitNode) then
    begin
      FTreeView.RepaintNode(LHitInfo.HitNode);
    end;

    FHitInfo := LHitInfo;

    if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn) then
      FHitInfo.HitPositions := [hiOnItem, hiOnItemCheckbox];

    if IsMouseInToggleIcon(LHitInfo) then
      FHitInfo.HitPositions := [hiOnItem, hiOnNormalIcon];
  end;
end;

procedure TTreeViewPresenter.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LHitInfo: THitInfo;
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(FHitInfo.HitNode) then
  begin
    FTreeView.GetHitTestInfoAt(X, Y, False, LHitInfo);
    if (FHitInfo.HitNode = LHitInfo.HitNode)
      and (FHitInfo.HitColumn = LHitInfo.HitColumn)
      and Assigned(ColumnDefinitions) and (LHitInfo.HitColumn > -1)
      and (LHitInfo.HitColumn < ColumnDefinitions.Count) then
    begin
      LItem := GetNodeItem(FTreeView, LHitInfo.HitNode);
      LItemTemplate := GetItemTemplate(LItem);
      LColumnDefinition := ColumnDefinitions[LHitInfo.HitColumn];

      if Assigned(LItemTemplate) then
      begin
        if IsMouseInCheckBox(LHitInfo.HitNode, LHitInfo.HitColumn)
          and LColumnDefinition.AllowEdit then
        begin
          LItemTemplate.SetValue(LItem, LHitInfo.HitColumn,
            not LItemTemplate.GetValue(LItem, LHitInfo.HitColumn).AsBoolean);
        end;

        if (hiOnNormalIcon in FHitInfo.HitPositions)
          and IsMouseInToggleIcon(LHitInfo)
          and (LColumnDefinition.ToggleMode = tmClick) then
        begin
          ToggleIcon(LHitInfo.HitNode, LHitInfo.HitColumn);
        end;
      end;
    end;

    FTreeView.RepaintNode(FHitInfo.HitNode);
    if LHitInfo.HitNode <> FHitInfo.HitNode then
    begin
      FTreeView.RepaintNode(LHitInfo.HitNode);
    end;
  end;
end;

procedure TTreeViewPresenter.DoNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.SetText(LItem, Column, NewText);
  end;
end;

procedure TTreeViewPresenter.DoNodeMoved(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
  LItems: IList;
begin
  LItem := GetNodeItem(Sender, Node);

  if Node.Parent <> FCurrentNode then
  begin
    LItems := GetNodeItems(Sender, FCurrentNode);
    if Assigned(LItems) then
    begin
      LItems.Extract(LItem);
    end;
  end;

  if Sender.GetNodeLevel(Node) = 0 then
  begin
    LItems := View.ItemsSource;
  end
  else
  begin
    LItems := GetNodeItems(Sender, Node.Parent);
  end;

  if Assigned(LItems) then
  begin
    if Node.Parent <> FCurrentNode then
    begin
      LItems.Insert(Node.Index, LItem);
    end
    else
    begin
      LItems.Move(LItems.IndexOf(LItem), Node.Index);
    end;
  end;
end;

procedure TTreeViewPresenter.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  LItem: TObject;
  LItemTemplate: IControlTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CDefaultCellRect,
      ImageList, dmPaintText, Sender.Selected[Node]);
  end;
end;

procedure TTreeViewPresenter.DoSelectedItemsChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FSelectionMode <> smNone) then
  begin
    if FCollectionChanging = 0 then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        if GetNodeItem(FTreeView, LNode) = Item then
        begin
          case Action of
            caAdd: FTreeView.Selected[LNode] := True;
            caRemove: FTreeView.Selected[LNode] := False;
          end;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;

    if FSyncMode and not FSyncing then
    try
      FSyncing := True;
      SetCheckedItems(FSelectedItems);
    finally
      FSyncing := False;
    end;
  end;
end;

procedure TTreeViewPresenter.DoSourceCollectionChanged(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
  LSelectedNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in ComponentState)
    and (FUpdateCount = 0) then
  begin
    case Action of
      caAdd:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        FTreeView.AddChild(LNode);
      end;

      caRemove:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        LNode := FTreeView.IterateSubtree(LNode, GetItemNode, Pointer(Item));

        // find node to select after deleting current node
        if FTreeView.Selected[LNode] and (FSelectionMode = smSingle) then
        begin
          LSelectedNode := FTreeView.GetNextSibling(LNode);

          if not Assigned(LSelectedNode) then
            LSelectedNode := FTreeView.GetPreviousSibling(LNode);
          if not Assigned(LSelectedNode) then
            LSelectedNode := LNode.Parent;
          if Assigned(LSelectedNode) then
            FTreeView.Selected[LSelectedNode] := True;
        end;

        FTreeView.DeleteNode(LNode);
      end;

      caReplace:
      begin
        LNode := FTreeView.IterateSubtree(nil, GetItemsNode, Pointer(Sender));
        FTreeView.Sort(LNode, FTreeView.Header.SortColumn, FTreeView.Header.SortDirection);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DrawCheckBox(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Boolean);
var
  LThemedButton: TThemedButton;
  LCheckBoxRect: TRect;
  LDetails: TThemedElementDetails;
  LState: Cardinal;
begin
  LCheckBoxRect := CalcCheckBoxRect(CellRect);

  if Value then
  begin
    LThemedButton := tbCheckBoxCheckedNormal;
  end
  else
  begin
    LThemedButton := tbCheckBoxUncheckedNormal;
  end;

  if IsMouseInCheckBox(Node, Column) then
  begin
    Inc(LThemedButton);
  end;

  if (FHitInfo.HitNode = Node) and (FHitInfo.HitColumn = Column)
    and (hiOnItemCheckbox in FHitInfo.HitPositions)
    and (GetAsyncKeyState(VK_LBUTTON) <> 0)
    and ColumnDefinitions[FHitInfo.HitColumn].AllowEdit then
  begin
    if Value then
    begin
      LThemedButton := tbCheckBoxCheckedPressed;
    end
    else
    begin
      LThemedButton := tbCheckBoxUncheckedPressed;
    end;
  end;

  if StyleServices.Enabled and
    (toThemeAware in FTreeView.TreeOptions.PaintOptions) then
  begin
    LDetails := StyleServices.GetElementDetails(LThemedButton);
    StyleServices.DrawElement(TargetCanvas.Handle, LDetails, LCheckBoxRect);
  end
  else
  begin
    LState := DFCS_BUTTONCHECK;
    if LThemedButton in [tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot] then
    begin
      LState := LState or DFCS_CHECKED;
    end;

    DrawFrameControl(TargetCanvas.Handle, LCheckBoxRect, DFC_BUTTON, LState);
  end;
end;

procedure TTreeViewPresenter.DrawImage(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Integer);
var
  LRect: TRect;
begin
  if Assigned(ImageList) then
  begin
    LRect := CalcImageRect(CellRect);
    ImageList.Draw(TargetCanvas, LRect.Left, LRect.Top, Value);
  end;
end;

procedure TTreeViewPresenter.DrawProgressBar(TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect; Value: Integer);
var
  LDetails: TThemedElementDetails;
begin
  if StyleServices.Enabled and
    (toThemeAware in FTreeView.TreeOptions.PaintOptions) then
  begin
    InflateRect(CellRect, -1, -1);
    LDetails := StyleServices.GetElementDetails(tpBar);
    StyleServices.DrawElement(TargetCanvas.Handle, LDetails, CellRect, nil);
    InflateRect(CellRect, -2, -2);
    CellRect.Right := CellRect.Left + Trunc(RectWidth(CellRect) * Value / 100);
    LDetails := StyleServices.GetElementDetails(tpChunk);
    StyleServices.DrawElement(TargetCanvas.Handle, LDetails, CellRect, nil);
  end
  else
  begin
    InflateRect(CellRect, -1, -1);
    FProgressBar.Position := Value;
    FProgressBar.Height := RectHeight(CellRect);
    FProgressBar.Width := RectWidth(CellRect);
    FProgressBar.PaintTo(TargetCanvas, CellRect.Left, 1);
  end;
end;

procedure TTreeViewPresenter.EndUpdate;
begin
  inherited;
  FTreeView.EndUpdate();
end;

procedure TTreeViewPresenter.ExpandNode(Node: PVirtualNode);
begin
  while Assigned(Node) do
  begin
    if [vsChecking..vsExpanded] * Node.States = [] then
    begin
      FTreeView.Expanded[Node] := True;
    end;
    Node := Node.Parent;
  end;
end;

procedure TTreeViewPresenter.FullCollapse;
begin
  FTreeView.FullCollapse();
end;

procedure TTreeViewPresenter.FullExpand;
begin
  FTreeView.FullExpand();
end;

function TTreeViewPresenter.GetCheckedItem: TObject;
begin
  if FCheckedItems.Count > 0 then
  begin
    Result := FCheckedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetCheckedItems: IList<TObject>;
begin
  UpdateCheckedItems();
  Result := FCheckedItems;
end;

function TTreeViewPresenter.GetCurrentItem: TObject;
begin
  Result := GetSelectedItem();
end;

function TTreeViewPresenter.GetExpandedItems: IList<TObject>;
begin
  UpdateExpandedItems();
  Result := FExpandedItems;
end;

procedure TTreeViewPresenter.GetItemNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Abort := GetNodeItem(Sender, Node) = TObject(Data);
end;

procedure TTreeViewPresenter.GetItemsNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Sender.GetNodeData(Node));
  Abort := Assigned(LNodeData) and (LNodeData.ItemsAsObject = TObject(Data));
end;

function TTreeViewPresenter.GetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.Item;
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetNodeItems(Tree: TBaseVirtualTree;
  Node: PVirtualNode): IList;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.Items;
  end
  else
  begin
    if Node = Tree.RootNode then
    begin
      Result := View.ItemsSource;
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TTreeViewPresenter.GetNodeItemsAsObject(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    Result := LNodeData.ItemsAsObject;
  end
  else
  begin
    if Node = Tree.RootNode then
    begin
      Result := View.ItemsSource.AsObject;
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TTreeViewPresenter.GetParentItem(const Level: Integer): TObject;
var
  LLevel: Integer;
  LNode: PVirtualNode;
begin
  Result := GetNodeItem(FTreeView, FCurrentNode);

  LLevel := 0;
  LNode := FCurrentNode;
  while Assigned(LNode) and (LLevel < Level) do
  begin
    LNode := LNode.Parent;
    Result := GetNodeItem(FTreeView, LNode);
    Inc(LLevel);
  end;
end;

function TTreeViewPresenter.GetSelectedItem: TObject;
begin
  if (FSelectedItems.Count > 0) and (FSelectionMode <> smNone) then
  begin
    Result := FSelectedItems[0];
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetSelectedItems: IList<TObject>;
begin
  if (FSelectedItems.Count > 0) and (FSelectionMode = smNone) then
  begin
    FSelectedItems.Clear;
  end;
  Result := FSelectedItems;
end;

procedure TTreeViewPresenter.InitColumns;
var
  i: Integer;
begin
  if Assigned(FTreeView) and UseColumnDefinitions then
  begin
    FTreeView.Header.Columns.Clear;
    if Assigned(ColumnDefinitions) then
    begin
      FTreeView.Header.AutoSizeIndex := ColumnDefinitions.MainColumnIndex;
      if FTreeView.Header.AutoSizeIndex = -1 then
      begin
        FTreeView.Header.Options := FTreeView.Header.Options - [hoAutoResize];
      end
      else
      begin
        FTreeView.Header.Options := FTreeView.Header.Options + [hoAutoResize];
      end;
      for i := 0 to Pred(ColumnDefinitions.Count) do
      begin
        with FTreeView.Header.Columns.Add do
        begin
          Alignment := ColumnDefinitions[i].Alignment;
          MinWidth := ColumnDefinitions[i].MinWidth;
          Text := ColumnDefinitions[i].Caption;
          Width := ColumnDefinitions[i].Width;
          Options := Options + [coUseCaptionAlignment];
          if not ColumnDefinitions[i].Visible then
          begin
            Options := Options - [coVisible];
          end;
        end;
      end;
      if FSorting then
      begin
        FTreeView.Header.SortColumn := ColumnDefinitions.MainColumnIndex;
      end
      else
      begin
        FTreeView.Header.SortColumn := -1;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.InitControl;
begin
  if Assigned(FTreeView) and ([csDesigning, csDestroying] * ComponentState = []) then
  begin
    FTreeView.Images := ImageList;
    FTreeView.NodeDataSize := SizeOf(TNodeData);
    FTreeView.PopupMenu := PopupMenu;

    InitColumns();
    InitProperties();
    InitEvents();
    ResetRootNodeCount();
  end;
end;

procedure TTreeViewPresenter.InitEvents;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    FTreeView.OnAfterCellPaint := DoAfterCellPaint;
    FTreeView.OnBeforeCellPaint := DoBeforeCellPaint;
    FTreeView.OnChange := DoChange;
    FTreeView.OnChecked := DoChecked;
    FTreeView.OnCollapsed := DoCollapsed;
    FTreeView.OnCompareNodes := DoCompareNodes;
    FTreeView.OnDblClick := DoDblClick;
    FTreeView.OnDragAllowed := DoDragAllowed;
    FTreeView.OnDragDrop := DoDragDrop;
    FTreeView.OnDragOver := DoDragOver;
    FTreeView.OnEdited := DoEdited;
    FTreeView.OnEditing := DoEditing;
    FTreeView.OnExpanded := DoExpanded;
    FTreeView.OnFocusChanged := DoFocusChanged;
    FTreeView.OnFreeNode := DoFreeNode;
    FTreeView.OnGetHint := DoGetHint;
    FTreeView.OnGetImageIndex := DoGetImageIndex;
    FTreeView.OnGetText := DoGetText;
    FTreeView.OnHeaderClick := DoHeaderClick;
    FTreeView.OnIncrementalSearch := DoIncrementalSearch;
    FTreeView.OnInitNode := DoInitNode;
    FTreeView.OnKeyDown := DoKeyDown;
    FTreeView.OnMouseDown := DoMouseDown;
    FTreeView.OnMouseMove := DoMouseMove;
    FTreeView.OnMouseUp := DoMouseUp;
    FTreeView.OnNewText := DoNewText;
    FTreeView.OnNodeMoved := DoNodeMoved;
    FTreeView.OnPaintText := DoPaintText;
  end;
end;

procedure TTreeViewPresenter.InitProperties;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    if FAllowMove then
    begin
      FTreeView.DragMode := dmAutomatic;
    end
    else
    begin
      FTreeView.DragMode := dmManual;
    end;

    if FCheckSupport <> csNone then
    begin
      FTreeView.TreeOptions.MiscOptions :=
        FTreeView.TreeOptions.MiscOptions + [toCheckSupport];
    end
    else
    begin
      FTreeView.TreeOptions.MiscOptions :=
        FTreeView.TreeOptions.MiscOptions - [toCheckSupport];
    end;

    if FListMode then
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions - [toShowButtons, toShowRoot, toShowTreeLines];
    end
    else
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions + [toShowButtons, toShowRoot, toShowTreeLines];
    end;

    if FSelectionMode = smSingle then
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toMultiSelect];
    end
    else
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toMultiSelect];
    end;

    if FSelectionMode = smLevel then
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toLevelSelectConstraint];
    end
    else
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toLevelSelectConstraint];
    end;

    if FSelectionMode = smNone then
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions + [toHideSelection, toAlwaysHideSelection];
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toDisableDrawSelection];
    end
    else
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions - [toHideSelection, toAlwaysHideSelection];
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toDisableDrawSelection];
    end;

    if FShowHeader then
    begin
      FTreeView.Header.Options := FTreeView.Header.Options + [hoVisible];
    end
    else
    begin
      FTreeView.Header.Options := FTreeView.Header.Options - [hoVisible];
    end;

    if FSorting then
    begin
      FTreeView.TreeOptions.AutoOptions :=
        FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] + [toAutoSort];
    end
    else
    begin
      FTreeView.TreeOptions.AutoOptions :=
        FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] - [toAutoSort];
    end;

    FTreeView.HintMode := hmHintAndDefault;
    FTreeView.IncrementalSearch := isAll;
    FTreeView.ShowHint := True;

    FTreeView.TreeOptions.MiscOptions :=
      FTreeView.TreeOptions.MiscOptions - [toToggleOnDblClick];
    FTreeView.TreeOptions.PaintOptions :=
      FTreeView.TreeOptions.PaintOptions + [toHideFocusRect, toUseExplorerTheme];
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions + [toExtendedFocus, toFullRowSelect, toRightClickSelect];
  end;
end;

function TTreeViewPresenter.IsMouseInCheckBox(Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
var
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
  LRect: TRect;
begin
  if Assigned(Node) and Assigned(ColumnDefinitions) and (Column > -1)
    and (ColumnDefinitions[Column].ColumnType = TColumnType.ctCheckBox) then
  begin
    LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
    FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, False, LHitInfo);
    LRect := FTreeView.GetDisplayRect(Node, Column, False);
    LRect := CalcCheckBoxRect(LRect);
    Result := PtInRect(LRect, LCursorPos);
  end
  else
  begin
    Result := False;
  end;
end;

function TTreeViewPresenter.IsMouseInToggleIcon(HitInfo: THitInfo): Boolean;
var
  LCursorPos: TPoint;
  LRect: TRect;
begin
  if Assigned(ColumnDefinitions) and (HitInfo.HitColumn > -1)
    and (HitInfo.HitColumn < ColumnDefinitions.Count) then
  begin
    if ColumnDefinitions[HitInfo.HitColumn].ColumnType = ctImage then
    begin
      LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
      LRect := FTreeView.GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, False);
      LRect := CalcImageRect(LRect);
      if PtInRect(LRect, LCursorPos) then
      begin
        Include(HitInfo.HitPositions, hiOnNormalIcon);
      end;
    end;

    Result := (hiOnNormalIcon in HitInfo.HitPositions)
      and (ColumnDefinitions[HitInfo.HitColumn].ToggleMode <> tmNone);
  end
  else
  begin
    Result := False;
  end;
end;

procedure TTreeViewPresenter.ReadMultiSelect(Reader: TReader);
begin
  if Reader.ReadBoolean then
  begin
    FSelectionMode := smMulti;
  end
  else
  begin
    FSelectionMode := smSingle;
  end;
end;

procedure TTreeViewPresenter.Refresh;
var
  LCheckedItems: IList<TObject>;
  LExpandedItems: IList<TObject>;
  LSelectedItems: IList<TObject>;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    LCheckedItems := TList<TObject>.Create();
    LCheckedItems.AddRange(CheckedItems);
    LExpandedItems := TList<TObject>.Create();
    LExpandedItems.AddRange(ExpandedItems);
    LSelectedItems := TList<TObject>.Create();
    LSelectedItems.AddRange(SelectedItems);
    ResetRootNodeCount();
    CheckedItems := LCheckedItems;
    ExpandedItems := LExpandedItems;
    SelectedItems := LSelectedItems;
  end;
end;

procedure TTreeViewPresenter.ResetRootNodeCount;
begin
  if Assigned(FTreeView) then
  begin
    if Assigned(View.ItemsSource) and Assigned(View.ItemTemplate) then
    begin
      FTreeView.Clear;
      FTreeView.RootNodeCount := View.ItemTemplate.GetItemCount(View.ItemsSource.AsObject);
    end
    else
    begin
      FTreeView.RootNodeCount := 0;
    end;
  end;
end;

procedure TTreeViewPresenter.SetCheckedItem(const Value: TObject);
begin
  FCheckedItems.Clear();
  if Assigned(Value) then
  begin
    FCheckedItems.Add(Value);
  end;
  SetCheckedItems(FCheckedItems);
end;

procedure TTreeViewPresenter.SetCheckedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and Value.Contains(LItem) then
      begin
        FTreeView.CheckState[LNode] := csCheckedNormal;
      end
      else
      begin
        FTreeView.CheckState[LNode] := csUncheckedNormal;
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.SetCheckSupport(const Value: TCheckSupport);
begin
  FCheckSupport := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetCurrentItem(const Value: TObject);
begin
  SetSelectedItem(Value);
end;

procedure TTreeViewPresenter.SetExpandedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  FTreeView.FullCollapse();
  if Assigned(Value) and (Value.Count > 0) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and Value.Contains(LItem) then
      begin
        ExpandNode(LNode);
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.SetListMode(const Value: Boolean);
begin
  FListMode := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Item: TObject);
var
  LNodeData: PNodeData;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    LNodeData.Item := Item;
  end;
end;

procedure TTreeViewPresenter.SetNodeItems(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Items: IList);
var
  LNodeData: PNodeData;
  LCollectionChanged: IEvent<TCollectionChangedEvent>;
begin
  LNodeData := PNodeData(Tree.GetNodeData(Node));
  if Assigned(LNodeData) then
  begin
    if Assigned(LNodeData.Items) then
    begin
      LCollectionChanged := IEvent<TCollectionChangedEvent>(LNodeData.Items.OnCollectionChanged);
      LCollectionChanged.Remove(DoSourceCollectionChanged);
    end;
    LNodeData.Items := Items;
    if Assigned(LNodeData.Items) then
    begin
      LNodeData.ItemsAsObject := Items.AsObject;
      LCollectionChanged := IEvent<TCollectionChangedEvent>(LNodeData.Items.OnCollectionChanged);
      LCollectionChanged.Add(DoSourceCollectionChanged);
    end;
  end;
end;

procedure TTreeViewPresenter.SetSelectedItem(const Value: TObject);
begin
  if ((Value <> SelectedItem) or (SelectedItems.Count > 1))
    and (FSelectionMode <> smNone) then
  begin
    FSelectedItems.Clear();
    if Assigned(Value) then
    begin
      FSelectedItems.Add(Value);
    end;
    SetSelectedItems(FSelectedItems);
  end;
end;

procedure TTreeViewPresenter.SetSelectedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in FTreeView.ComponentState)
    and (FSelectionMode <> smNone) then
  begin
    FTreeView.BeginUpdate();
    FTreeView.ClearSelection();
    if Assigned(Value) and (Value.Count > 0) then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and Value.Contains(LItem) then
        begin
          FTreeView.Selected[LNode] := True;
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
      LNode := FTreeView.GetFirstSelected();
      FTreeView.FocusedNode := LNode;
      if Assigned(LNode) and (FCollectionChanging = 0) then
      begin
        FTreeView.ScrollIntoView(LNode, True, True);
      end;
    end;
    FTreeView.EndUpdate();
  end;
end;

procedure TTreeViewPresenter.SetSelectionMode(const Value: TSelectionMode);
begin
  FSelectionMode := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetShowHeader(const Value: Boolean);
begin
  FShowHeader := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetSorting(const Value: Boolean);
begin
  FSorting := Value;
  if Assigned(FTreeView) and not FSorting then
  begin
    FTreeView.Header.SortColumn := -1;
    Refresh();
  end;
  InitProperties();
end;

procedure TTreeViewPresenter.SetTreeView(const Value: TVirtualStringTree);
begin
  FTreeView := Value;
  if not (csDesigning in ComponentState) then
  begin
    FProgressBar.Parent := FTreeView;
  end;
  InitControl();
end;

procedure TTreeViewPresenter.ToggleIcon(Node: PVirtualNode;
  Column: TColumnIndex);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LColumnDefinition: TColumnDefinition;
  LValue: TValue;

  procedure ToggleValue;
  begin
    if LValue.AsOrdinal < LValue.TypeData.MaxValue then
    begin
      TValue.Make(LValue.AsOrdinal + 1, LValue.TypeInfo, LValue);
    end
    else
    begin
      TValue.Make(LValue.TypeData.MinValue, LValue.TypeInfo, LValue);
    end;
  end;

begin
  LItem := GetNodeItem(FTreeView, Node);
  LColumnDefinition := ColumnDefinitions[Column];
  if LColumnDefinition.ColumnType = ctImage then
  begin
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      LValue := LItemTemplate.GetValue(LItem, Column);
      if LValue.IsOrdinal then
      begin
        ToggleValue;
        LItemTemplate.SetValue(LItem, Column, LValue);
      end;
    end;
  end
  else
  begin
    LColumnDefinition.ImageIndexPropertyExpression.Instance := LItem;
    LValue := LColumnDefinition.ImageIndexPropertyExpression.Value;
    if LValue.IsOrdinal then
    begin
      ToggleValue;
      LColumnDefinition.ImageIndexPropertyExpression.Value := LValue;
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateCheckedItems;
var
  LItem: TObject;
  LCheckedItems: IList<TObject>;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    Inc(FCollectionChanging);
    LCheckedItems := TList<TObject>.Create();
    try
      LNode := FTreeView.GetFirstChecked();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) then
        begin
          LCheckedItems.Add(LItem);
        end;
        LNode := FTreeView.GetNextChecked(LNode);
      end;

      Synchronize(FCheckedItems, LCheckedItems);
    finally
      Dec(FCollectionChanging);
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateExpandedItems;
var
  LItem: TObject;
  LExpandedItems: IList<TObject>;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    Inc(FCollectionChanging);
    LExpandedItems := TList<TObject>.Create();
    try
      LNode := FTreeView.GetFirstInitialized();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and FTreeView.Expanded[LNode] then
        begin
          LExpandedItems.Add(LItem);
        end;
        LNode := FTreeView.GetNextInitialized(LNode);
      end;

      Synchronize(FExpandedItems, LExpandedItems);
    finally
      Dec(FCollectionChanging);
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateSelectedItems;
var
  i: Integer;
  LItem: TObject;
  LSelectedItems: IList<TObject>;
  LSelectedNodes: TNodeArray;
begin
  if Assigned(FTreeView) and (FSelectionMode <> smNone) then
  begin
    Inc(FCollectionChanging);
    LSelectedItems := TList<TObject>.Create();
    try
      LSelectedNodes := FTreeView.GetSortedSelection(False);

      for i := Low(LSelectedNodes) to High(LSelectedNodes) do
      begin
        LItem := GetNodeItem(FTreeView, LSelectedNodes[i]);
        if Assigned(LItem) then
        begin
          LSelectedItems.Add(LItem);
        end;
      end;

      Synchronize(FSelectedItems, LSelectedItems);
    finally
      Dec(FCollectionChanging);
    end;
  end;
end;

initialization
  CheckBoxSize := GetSystemMetrics(SM_CYMENUCHECK);

end.
