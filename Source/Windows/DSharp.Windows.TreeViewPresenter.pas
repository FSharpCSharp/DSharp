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

unit DSharp.Windows.TreeViewPresenter;

interface

uses
  ActiveX,
  Classes,
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
  TCompareEvent = procedure(Sender: TObject; Item1, Item2: TObject;
    ColumnIndex: Integer; var Result: Integer) of object;
  TDragBeginEvent = procedure(Sender: TObject; var AllowDrag: Boolean) of object;
  TDragOverEvent = procedure(Sender: TObject; TargetItem: TObject;
    var AllowDrop: Boolean) of object;
  TDragDropEvent = procedure(Sender: TObject; TargetItem: TObject;
    DragOperation: TDragOperation; var DropMode: TDropMode) of object;

  TCheckSupport = (csNone, csSimple, csTriState);

  TTreeViewPresenter = class(TCustomPresenter)
  private    
    FAllowMove: Boolean;
    FCheckedItems: IList<TObject>;
    FCheckSupport: TCheckSupport;
    FCurrentNode: PVirtualNode;
    FExpandedItems: IList<TObject>;
    FListMode: Boolean;
    FMultiSelect: Boolean;
    FOnCompare: TCompareEvent;
    FOnDragBegin: TDragBeginEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnSelectionChanged: TNotifyEvent;
    FSelectedItems: IList<TObject>;
    FSorting: Boolean;
    FTreeView: TVirtualStringTree;

    procedure DoAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure DoBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure DoChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    procedure DoNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure DoNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

    function GetCheckedItems: IList<TObject>;
    function GetExpandedItems: IList<TObject>;
    procedure GetItemNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetParentItem(const Level: Integer): TObject;
    function GetSelectedItem: TObject;
    function GetSelectedItems: IList<TObject>;

    procedure ResetRootNodeCount;

    procedure SetCheckedItems(const Value: IList<TObject>);
    procedure SetCheckSupport(const Value: TCheckSupport);
    procedure SetExpandedItems(const Value: IList<TObject>);
    procedure SetListMode(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode; Item: TObject);
    procedure SetSelectedItem(const Value: TObject);
    procedure SetSelectedItems(const Value: IList<TObject>);
    procedure SetSorting(const Value: Boolean);
    procedure SetTreeView(const Value: TVirtualStringTree);

    procedure UpdateCheckedItems;
    procedure UpdateExpandedItems;
    procedure UpdateSelectedItems;
  protected
    procedure DoDblClick(Sender: TObject); override;
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
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

    procedure DeleteItems(Items: IList<TObject>);
    procedure FullCollapse;
    procedure FullExpand;

    procedure Refresh; override;

    property CheckedItems: IList<TObject> read GetCheckedItems write SetCheckedItems;
    property ExpandedItems: IList<TObject> read GetExpandedItems write SetExpandedItems;
    property ParentItem[const Level: Integer]: TObject read GetParentItem;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property SelectedItems: IList<TObject> read GetSelectedItems write SetSelectedItems;
  published    
    property AllowMove: Boolean read FAllowMove write FAllowMove default True;
    property CheckSupport: TCheckSupport read FCheckSupport write SetCheckSupport default csNone;
    property ListMode: Boolean read FListMode write SetListMode default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OnCompare: TCompareEvent read FOnCompare write FOnCompare;
    property OnDragBegin: TDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property Sorting: Boolean read FSorting write SetSorting default True;
    property TreeView: TVirtualStringTree read FTreeView write SetTreeView;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.Windows.ColumnDefinitions.ControlTemplate,
  DSharp.Windows.ControlTemplates,
  Math,
  Windows;

const
  CDefaultCellRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{ TTreeViewPresenter }

constructor TTreeViewPresenter.Create(AOwner: TComponent);
begin
  FCheckedItems := TList<TObject>.Create();
  FExpandedItems := TList<TObject>.Create();
  FSelectedItems := TList<TObject>.Create();
  inherited;
  FAllowMove := True;
  FSorting := True;
end;

destructor TTreeViewPresenter.Destroy;
begin
  inherited;
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
      if Assigned(LItem) and (Items.IndexOf(LItem) > -1) then
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
  LItemTemplate: IControlTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, ImageList,
      dmAfterCellPaint, Sender.Selected[Node]);
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
  UpdateSelectedItems();
  if Assigned(FOnSelectionChanged) then
  begin
    FOnSelectionChanged(Self);
  end;
end;

procedure TTreeViewPresenter.DoChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateCheckedItems();
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

  if not FListMode and Assigned(LHitInfo.HitNode)
    and ((hiOnNormalIcon in LHitInfo.HitPositions)
    or (not Assigned(OnDoubleClick) and not Assigned(Action))) then
  begin
    FTreeView.ToggleNode(LHitInfo.HitNode);
  end
  else
  begin
    if ([hiOnItemButton..hiOnItemCheckbox] * LHitInfo.HitPositions = [])
      and Assigned(LHitInfo.HitNode) then
    begin
      inherited;
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
begin
  LNode := Sender.DropTargetNode;
  LItem := GetNodeItem(Sender, LNode);
  if Assigned(LItem) then
  begin
    LSelectedNodes := Sender.GetSortedSelection(False);
    if ssCtrl in Shift then
    begin
      if Assigned(FOnDragDrop) then
      begin
        FOnDragDrop(Sender, LItem, doCopy, Mode);
      end;
      Sender.ReinitNode(LNode, True);
    end
    else
    begin
      if Assigned(FOnDragDrop) then
      begin
        FOnDragDrop(Sender, LItem, doMove, Mode);
      end;
      for i := Low(LSelectedNodes) to High(LSelectedNodes) do
      begin
        case Mode of
          dmAbove: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertBefore, False);
          dmOnNode: FTreeView.MoveTo(LSelectedNodes[i], LNode, amAddChildLast, False);
          dmBelow: FTreeView.MoveTo(LSelectedNodes[i], LNode, amInsertAfter, False);
        end;
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
begin
  LNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  LItem := GetNodeItem(Sender, LNode);
  case Mode of
    dmAbove, dmBelow: Accept := FAllowMove;
  end;
  if Assigned(LItem) and Assigned(FOnDragOver) then
  begin
    FOnDragOver(Sender, LItem, Accept);
  end;
end;

procedure TTreeViewPresenter.DoFilterNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  i: Integer;
  LItem: TObject;
begin
  LItem := GetNodeItem(Sender, Node);

  if Assigned(View.Filter) then
  begin
    Sender.IsFiltered[Node] := not View.Filter(LItem);
  end
  else
  begin
    Sender.IsFiltered[Node] := False;
  end;

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
    CellText := LItemTemplate.GetText(LItem, Column);
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
  LItemTemplate: IDataTemplate;
  LParentItem: TObject;
begin
  FCurrentNode := Node;
  DoPropertyChanged('ParentItem');

  if FCheckSupport = csTriState then
  begin
    Node.CheckType := ctTriStateCheckbox;
  end
  else
  begin
    Node.CheckType := ctCheckBox;
  end;

  if Assigned(ParentNode) then
  begin
    LParentItem := GetNodeItem(Sender, ParentNode);
    LItemTemplate := GetItemTemplate(LParentItem);
    LItem := LItemTemplate.GetItem(LParentItem, Node.Index);
  end
  else
  begin
    LItem := View.ItemTemplate.GetItem(View.ItemsSource as TObject, Node.Index);
  end;

  SetNodeItem(Sender, Node, LItem);

  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    Sender.ChildCount[Node] := LItemTemplate.GetItemCount(LItem);
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
  if (ssCtrl in Shift) and (FTreeView.Header.SortColumn = -1) then
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
          for i := Low(LNodes) to High(LNodes) do
          begin
            FTreeView.MoveTo(LNodes[i], LNodes[i].PrevSibling, amInsertBefore, False);
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
          for i := High(LNodes) downto Low(LNodes) do
          begin
            FTreeView.MoveTo(LNodes[i], LNodes[i].NextSibling, amInsertAfter, False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LCursorPos: TPoint;
  LHitInfo: THitInfo;
begin
  if not (ssDouble in Shift) then
  begin
    LCursorPos := FTreeView.ScreenToClient(Mouse.CursorPos);
    FTreeView.GetHitTestInfoAt(LCursorPos.X, LCursorPos.Y, False, LHitInfo);
    if not Assigned(LHitInfo.HitNode) and not FMultiSelect then
    begin
      FTreeView.ClearSelection();
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
  LItems: IList<TObject>;
  LItemTemplate: IDataTemplate;
begin
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    LItem := GetNodeItem(Sender, Node);
    View.ItemsSource.Move(View.ItemsSource.IndexOf(LItem), Node.Index);
  end
  else
  begin
    LItem := GetNodeItem(Sender, Node.Parent);
    LItemTemplate := GetItemTemplate(LItem);
    if Assigned(LItemTemplate) then
    begin
      LItems := LItemTemplate.GetItems(LItem);
      if Assigned(LItems) then
      begin
        LItem := GetNodeItem(Sender, Node);
        LItems.Move(LItems.IndexOf(LItem), Node.Index);
      end;
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

procedure TTreeViewPresenter.DoSourceCollectionChanged(Sender: TObject;
  Item: TObject; Action: TCollectionChangedAction);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    LNode := FTreeView.IterateSubtree(nil, GetItemNode, Pointer(Item));
    case Action of
      caAdd: ResetRootNodeCount;
      caRemove: ResetRootNodeCount;
      caReplace:
      begin
        if Assigned(LNode) then
        begin
          FTreeView.ReinitNode(LNode, True);
          FTreeView.InvalidateNode(LNode);
        end;
      end;
    end;
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

function TTreeViewPresenter.GetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
begin
  if Assigned(Tree) and Assigned(Node) and (Node <> FTreeView.RootNode) then
  begin
    Result := PPointer(Tree.GetNodeData(Node))^;
  end
  else
  begin
    Result := nil;
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
  if FSelectedItems.Count > 0 then
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
  Result := FSelectedItems;
end;

procedure TTreeViewPresenter.InitColumns;
var
  i: Integer;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.Header.Columns.Clear;
    if Assigned(ColumnDefinitions) then
    begin
      for i := 0 to Pred(ColumnDefinitions.Count) do
      begin
        with FTreeView.Header.Columns.Add do
        begin
          Alignment := ColumnDefinitions[i].Alignment;
          Text := ColumnDefinitions[i].Caption;
          Width := ColumnDefinitions[i].Width;
          Options := Options + [coUseCaptionAlignment];
        end;
      end;
      FTreeView.Header.SortColumn := ColumnDefinitions.MainColumnIndex;
    end;
    FTreeView.Header.Options := FTreeView.Header.Options + [hoVisible];
  end;
end;

procedure TTreeViewPresenter.InitControl;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.Images := ImageList;
    FTreeView.NodeDataSize := SizeOf(TObject);
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
    FTreeView.OnCompareNodes := DoCompareNodes;
    FTreeView.OnDblClick := DoDblClick;
    FTreeView.OnDragAllowed := DoDragAllowed;
    FTreeView.OnDragDrop := DoDragDrop;
    FTreeView.OnDragOver := DoDragOver;
    FTreeView.OnFocusChanged := DoFocusChanged;
    FTreeView.OnGetHint := DoGetHint;
    FTreeView.OnGetImageIndex := DoGetImageIndex;
    FTreeView.OnGetText := DoGetText;
    FTreeView.OnHeaderClick := DoHeaderClick;
    FTreeView.OnIncrementalSearch := DoIncrementalSearch;
    FTreeView.OnInitNode := DoInitNode;
    FTreeView.OnKeyDown := DoKeyDown;
    FTreeView.OnMouseDown := DoMouseDown;
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

    if FMultiSelect then
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions + [toMultiSelect];
    end
    else
    begin
      FTreeView.TreeOptions.SelectionOptions :=
        FTreeView.TreeOptions.SelectionOptions - [toMultiSelect];
    end;

    if FListMode then
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines];
    end
    else
    begin
      FTreeView.TreeOptions.PaintOptions :=
        FTreeView.TreeOptions.PaintOptions + [toShowRoot, toShowTreeLines];
    end;

    FTreeView.HintMode := hmHintAndDefault;
    FTreeView.IncrementalSearch := isAll;
    FTreeView.ShowHint := True;
    FTreeView.TreeOptions.AutoOptions :=
      FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] + [toAutoSort];
    FTreeView.TreeOptions.MiscOptions :=
      FTreeView.TreeOptions.MiscOptions - [toToggleOnDblClick];
    FTreeView.TreeOptions.PaintOptions :=
      FTreeView.TreeOptions.PaintOptions + [toHideFocusRect];
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions + [toExtendedFocus, toFullRowSelect, toRightClickSelect];
  end;
end;

procedure TTreeViewPresenter.Refresh;
var
  LCheckedItems: IList<TObject>;
  LExpandedItems: IList<TObject>;
  LSelectedItems: IList<TObject>;
begin
  if Assigned(FTreeView) then
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
      FTreeView.RootNodeCount := View.ItemTemplate.GetItemCount(View.ItemsSource as TObject);
    end
    else
    begin
      FTreeView.RootNodeCount := 0;
    end;
  end;
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
      if Assigned(LItem) and (Value.IndexOf(LItem) > -1) then
      begin
        FTreeView.CheckState[LNode] := csCheckedNormal;
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
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and (Value.IndexOf(LItem) > -1) then
      begin
        FTreeView.Expanded[LNode] := True;
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

procedure TTreeViewPresenter.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect := Value;
  InitProperties();
end;

procedure TTreeViewPresenter.SetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Item: TObject);
begin
  if Assigned(Tree) and Assigned(Node) then
  begin
    PPointer(Tree.GetNodeData(Node))^ := Item;
  end;
end;

procedure TTreeViewPresenter.SetSelectedItem(const Value: TObject);
begin
  FSelectedItems.Clear();
  FSelectedItems.Add(Value);
  SetSelectedItems(FSelectedItems);
end;

procedure TTreeViewPresenter.SetSelectedItems(const Value: IList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and not (csDestroying in FTreeView.ComponentState) then
  begin
    FTreeView.BeginUpdate();
    FTreeView.ClearSelection();
    if Assigned(Value) and (Value.Count > 0) then
    begin
      LNode := FTreeView.GetFirst();
      while Assigned(LNode) do
      begin
        LItem := GetNodeItem(FTreeView, LNode);
        if Assigned(LItem) and (Value.IndexOf(LItem) > -1) then
        begin
          FTreeView.Selected[LNode] := True;
          FTreeView.ScrollIntoView(LNode, True, True);
        end;
        LNode := FTreeView.GetNext(LNode);
      end;
    end;
    FTreeView.EndUpdate();
  end;
end;

procedure TTreeViewPresenter.SetSorting(const Value: Boolean);
begin
  FSorting := Value;
  if Assigned(FTreeView) and not FSorting then
  begin
    FTreeView.Header.SortColumn := -1;
    Refresh();
  end;
end;

procedure TTreeViewPresenter.SetTreeView(const Value: TVirtualStringTree);
begin
  FTreeView := Value;
  InitControl();
end;

procedure TTreeViewPresenter.UpdateCheckedItems;
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    FCheckedItems.Clear();
    LNode := FTreeView.GetFirstChecked();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) then
      begin
        FCheckedItems.Add(LItem);
      end;
      LNode := FTreeView.GetNextChecked(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateExpandedItems;
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) then
  begin
    FExpandedItems.Clear();
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and FTreeView.Expanded[LNode] then
      begin
        FExpandedItems.Add(LItem);
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
end;

procedure TTreeViewPresenter.UpdateSelectedItems;
var
  i: Integer;
  LItem: TObject;
  LSelectedNodes: TNodeArray;
begin
  if Assigned(FTreeView) then
  begin
    FSelectedItems.Clear();
    LSelectedNodes := FTreeView.GetSortedSelection(False);

    for i := Low(LSelectedNodes) to High(LSelectedNodes) do
    begin
      LItem := GetNodeItem(FTreeView, LSelectedNodes[i]);
      if Assigned(LItem) then
      begin
        FSelectedItems.Add(LItem);
      end;
    end;
  end;

  DoPropertyChanged('View');
  DoPropertyChanged('SelectedItem');
  DoPropertyChanged('SelectedItems');
end;

end.
