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
  DSharp.Bindings,
  DSharp.Bindings.Collections,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Core.Events,
  DSharp.Windows.ColumnDefinitions,
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
    DragOperation: TDragOperation) of object;

  TCheckSupport = (csNone, csSimple, csTriState);

  TTreeViewPresenter = class(TComponent, ICollectionView, INotifyPropertyChanged)
  private
    FAllowMove: Boolean;
    FCheckedItems: TList<TObject>;
    FCheckSupport: TCheckSupport;
    FColumnDefinitions: TColumnDefinitions;
    FExpandedItems: TList<TObject>;
    FFilter: TPredicate<TObject>;
    FImageList: TImageList;
    FItemsSource: TList<TObject>;
    FItemTemplate: IDataTemplate;
    FListMode: Boolean;
    FMultiSelect: Boolean;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    FOnCompare: TCompareEvent;
    FOnDoubleClick: TNotifyEvent;
    FOnDragBegin: TDragBeginEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    FOnSelectionChanged: TNotifyEvent;
    FPopupMenu: TPopupMenu;
    FSelectedItems: TList<TObject>;
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
    procedure DoCurrentItemPropertyChanged(Sender: TObject;
      PropertyName: string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoDblClick(Sender: TObject);
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
    procedure DoGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure DoHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure DoInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction);

    function GetCheckedItems: TList<TObject>;
    function GetCurrentItem: TObject;
    function GetExpandedItems: TList<TObject>;
    function GetFilter: TPredicate<TObject>;
    procedure GetItemNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    function GetItemsSource: TList<TObject>;
    function GetItemTemplate: IDataTemplate; overload;
    function GetItemTemplate(const Item: TObject): IDataTemplate; overload;
    function GetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode): TObject;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetSelectedItem: TObject;
    function GetSelectedItems: TList<TObject>;

    procedure InitColumns;
    procedure InitEvents;
    procedure InitTreeOptions;

    procedure ResetRootNodeCount;

    procedure SetCheckSupport(const Value: TCheckSupport);
    procedure SetColumnDefinitions(const Value: TColumnDefinitions);
    procedure SetCurrentItem(const Value: TObject);
    procedure SetExpandedItems(const Value: TList<TObject>);
    procedure SetFilter(const Value: TPredicate<TObject>);
    procedure SetImageList(const Value: TImageList);
    procedure SetItemsSource(const Value: TList<TObject>);
    procedure SetItemTemplate(const Value: IDataTemplate);
    procedure SetListMode(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetNodeItem(Tree: TBaseVirtualTree; Node: PVirtualNode; Item: TObject);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetSelectedItem(const Value: TObject);
    procedure SetSelectedItems(const Value: TList<TObject>);
    procedure SetTreeView(const Value: TVirtualStringTree);

    procedure UpdateCheckedItems;
    procedure UpdateExpandedItems;
    procedure UpdateSelectedItems;
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DeleteItems(Items: TList<TObject>);
    procedure FullCollapse;
    procedure FullExpand;

    procedure Refresh;

    property CheckedItems: TList<TObject> read GetCheckedItems;
    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property ExpandedItems: TList<TObject> read GetExpandedItems write SetExpandedItems;
    property Filter: TPredicate<TObject> read GetFilter write SetFilter;
    property ItemsSource: TList<TObject> read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
    property SelectedItem: TObject read GetSelectedItem write SetSelectedItem;
    property SelectedItems: TList<TObject> read GetSelectedItems write SetSelectedItems;
  published
    property AllowMove: Boolean read FAllowMove write FAllowMove default True;
    property CheckSupport: TCheckSupport read FCheckSupport write SetCheckSupport default csNone;
    property ColumnDefinitions: TColumnDefinitions
      read FColumnDefinitions write SetColumnDefinitions;
    property ImageList: TImageList read FImageList write SetImageList;
    property ListMode: Boolean read FListMode write SetListMode default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent> read GetOnCollectionChanged;
    property OnCompare: TCompareEvent read FOnCompare write FOnCompare;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property OnDragBegin: TDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property TreeView: TVirtualStringTree read FTreeView write SetTreeView;
  end;

implementation

uses
  DSharp.Windows.ColumnDefinitions.DataTemplate,
  Windows;

const
  CDefaultCellRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  PObject = ^TObject;

{ TTreeViewPresenter }

constructor TTreeViewPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FAllowMove := True;
  FCheckedItems := TList<TObject>.Create();
  FExpandedItems := TList<TObject>.Create();
  FSelectedItems := TList<TObject>.Create();
  FOnCollectionChanged.Add(DoSourceCollectionChanged);

  FColumnDefinitions := TColumnDefinitions.Create(Self);
  FItemTemplate := TColumnDefinitionsDataTemplate.Create(FColumnDefinitions);
end;

destructor TTreeViewPresenter.Destroy;
begin
  if Assigned(FColumnDefinitions) and (FColumnDefinitions.Owner = Self) then
  begin
    FColumnDefinitions.Free();
  end;
  FCheckedItems.Free();
  FExpandedItems.Free();
  FSelectedItems.Free();
  inherited;
end;

procedure TTreeViewPresenter.DeleteItems(Items: TList<TObject>);
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
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, FImageList, dmAfterCellPaint);
  end;
end;

procedure TTreeViewPresenter.DoBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CellRect, FImageList, dmBeforeCellPaint);
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
  LItemTemplate1, LItemTemplate2: IDataTemplate;
begin
  LItem1 := GetNodeItem(Sender, Node1);
  LItem2 := GetNodeItem(Sender, Node2);
  LItemTemplate1 := GetItemTemplate(LItem1);
  LItemTemplate2 := GetItemTemplate(LItem2);

  if Assigned(FOnCompare) then
  begin
    FOnCompare(Self, LItem1, LItem2, Column, Result);
  end
  else
  begin
    // Using item template to sort
    if Assigned(LItemTemplate1) and Assigned(LItemTemplate2) then
    begin
      Result := CompareText(LItemTemplate1.GetText(LItem1, Column),
        LItemTemplate2.GetText(LItem2, Column));
    end;
  end;
end;

procedure TTreeViewPresenter.DoCurrentItemPropertyChanged(Sender: TObject;
  PropertyName: string; UpdateTrigger: TUpdateTrigger);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and (FTreeView.SelectedCount > 0) then
  begin
    for LNode in FTreeView.GetSortedSelection(True) do
    begin
      FTreeView.InvalidateNode(LNode);
      FTreeView.SortTree(FTreeView.Header.SortColumn, FTreeView.Header.SortDirection, False);
      DoFilterNode(FTreeView, LNode);
      LItem := GetNodeItem(FTreeView, LNode);
      DoSourceCollectionChanged(Sender, LItem, caReplace);
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

  if not FListMode and ((hiOnNormalIcon in LHitInfo.HitPositions)
    or not Assigned(FOnDoubleClick)) then
  begin
    FTreeView.ToggleNode(LHitInfo.HitNode);
  end
  else
  begin
    if ([hiOnItemButton..hiOnItemCheckbox] * LHitInfo.HitPositions = [])
      and Assigned(LHitInfo.HitNode) then
    begin
      if Assigned(FOnDoubleClick) then
      begin
        FOnDoubleClick(Self);
      end;
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
        FOnDragDrop(Sender, LItem, doCopy);
      end;
      Sender.ReinitNode(LNode, True);
    end
    else
    begin
      if Assigned(FOnDragDrop) then
      begin
        FOnDragDrop(Sender, LItem, doMove);
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

  if Assigned(FFilter) then
  begin
    Sender.IsFiltered[Node] := not FFilter(LItem);
  end
  else
  begin
    Sender.IsFiltered[Node] := False;
  end;

  if Assigned(FColumnDefinitions) then
  begin
    for i := 0 to Pred(FColumnDefinitions.Count) do
    begin
      if Assigned(FColumnDefinitions[i].Filter) then
      begin
        Sender.IsFiltered[Node] := Sender.IsFiltered[Node]
          or not FColumnDefinitions[i].Filter(LItem);
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

procedure TTreeViewPresenter.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    ImageIndex := LItemTemplate.GetImageIndex(LItem, Column);
  end;
end;

procedure TTreeViewPresenter.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
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

procedure TTreeViewPresenter.DoInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
  LParentItem: TObject;
begin
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
    LItem := FItemsSource[Node.Index];
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

procedure TTreeViewPresenter.DoNodeMoved(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LItem: TObject;
  LItems: TList<TObject>;
  LItemTemplate: IDataTemplate;
begin
  if Sender.GetNodeLevel(Node) = 0 then
  begin
    LItem := GetNodeItem(Sender, Node);
    FItemsSource.Move(FItemsSource.IndexOf(LItem), Node.Index);
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
  LItemTemplate: IDataTemplate;
begin
  LItem := GetNodeItem(Sender, Node);
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LItem, Column, TargetCanvas, CDefaultCellRect, FImageList, dmPaintText);
  end;
end;

procedure TTreeViewPresenter.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
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
        FTreeView.ReinitNode(LNode, True);
        FTreeView.InvalidateNode(LNode);
      end;
    end;
  end;

  DoPropertyChanged('ItemsSource');
end;

procedure TTreeViewPresenter.FullCollapse;
begin
  FTreeView.FullCollapse();
end;

procedure TTreeViewPresenter.FullExpand;
begin
  FTreeView.FullExpand();
end;

function TTreeViewPresenter.GetCheckedItems: TList<TObject>;
begin
  UpdateCheckedItems();
  Result := FCheckedItems;
end;

function TTreeViewPresenter.GetCurrentItem: TObject;
begin
  Result := GetSelectedItem();
end;

function TTreeViewPresenter.GetExpandedItems: TList<TObject>;
begin
  UpdateExpandedItems();
  Result := FExpandedItems;
end;

function TTreeViewPresenter.GetFilter: TPredicate<TObject>;
begin
  Result := FFilter;
end;

procedure TTreeViewPresenter.GetItemNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Abort := GetNodeItem(Sender, Node) = TObject(Data);
end;

function TTreeViewPresenter.GetItemsSource: TList<TObject>;
begin
  Result := FItemsSource;
end;

function TTreeViewPresenter.GetItemTemplate: IDataTemplate;
begin
  Result := FItemTemplate;
end;

function TTreeViewPresenter.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  Result := nil;

  if Assigned(FItemTemplate) then
  begin
    Result := FItemTemplate.GetItemTemplate(Item);
  end;
end;

function TTreeViewPresenter.GetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode): TObject;
begin
  if Assigned(Tree) and Assigned(Node) then
  begin
    Result := PObject(Tree.GetNodeData(Node))^;
  end
  else
  begin
    Result := nil;
  end;
end;

function TTreeViewPresenter.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TTreeViewPresenter.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
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

function TTreeViewPresenter.GetSelectedItems: TList<TObject>;
begin
  Result := FSelectedItems;
end;

procedure TTreeViewPresenter.InitColumns;
var
  i: Integer;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    FTreeView.Header.Columns.Clear;
    if Assigned(FColumnDefinitions) then
    begin
      for i := 0 to Pred(FColumnDefinitions.Count) do
      begin
        with FTreeView.Header.Columns.Add do
        begin
          Text := FColumnDefinitions[i].Caption;
          Width := FColumnDefinitions[i].Width;
        end;
      end;
    end;
    FTreeView.Header.Options := FTreeView.Header.Options + [hoVisible];
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
    FTreeView.OnGetImageIndex := DoGetImageIndex;
    FTreeView.OnGetText := DoGetText;
    FTreeView.OnHeaderClick := DoHeaderClick;
    FTreeView.OnInitNode := DoInitNode;
    FTreeView.OnKeyDown := DoKeyDown;
    FTreeView.OnMouseDown := DoMouseDown;
    FTreeView.OnNodeMoved := DoNodeMoved;
    FTreeView.OnPaintText := DoPaintText;
  end;
end;

procedure TTreeViewPresenter.InitTreeOptions;
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

    FTreeView.TreeOptions.AutoOptions :=
      FTreeView.TreeOptions.AutoOptions - [toAutoDeleteMovedNodes] + [toAutoSort];
    FTreeView.TreeOptions.MiscOptions :=
      FTreeView.TreeOptions.MiscOptions - [toToggleOnDblClick];
    FTreeView.TreeOptions.PaintOptions :=
      FTreeView.TreeOptions.PaintOptions + [toHideFocusRect];
    FTreeView.TreeOptions.SelectionOptions :=
      FTreeView.TreeOptions.SelectionOptions + [toFullRowSelect, toRightClickSelect];
  end;
end;

procedure TTreeViewPresenter.Loaded;
begin
  inherited;
  InitColumns();
end;

procedure TTreeViewPresenter.Refresh;
begin
  ResetRootNodeCount();
end;

procedure TTreeViewPresenter.ResetRootNodeCount;
begin
  if Assigned(FTreeView) then
  begin
    if Assigned(FItemsSource) then
    begin
      FTreeView.Clear;
      FTreeView.RootNodeCount := FItemsSource.Count;
    end
    else
    begin
      FTreeView.RootNodeCount := 0;
    end;
  end;
end;

procedure TTreeViewPresenter.SetCheckSupport(const Value: TCheckSupport);
begin
  FCheckSupport := Value;
  InitTreeOptions();
end;

procedure TTreeViewPresenter.SetColumnDefinitions(
  const Value: TColumnDefinitions);
begin
  if Assigned(FColumnDefinitions) and (FColumnDefinitions.Owner = Self) then
  begin
    FColumnDefinitions.Free();
  end;
  FColumnDefinitions := Value;
  InitColumns();
end;

procedure TTreeViewPresenter.SetCurrentItem(const Value: TObject);
begin
  SetSelectedItem(Value);
end;

procedure TTreeViewPresenter.SetExpandedItems(const Value: TList<TObject>);
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

procedure TTreeViewPresenter.SetFilter(const Value: TPredicate<TObject>);
begin
  FFilter := Value;
  FTreeView.ReinitChildren(FTreeView.RootNode, True);
end;

procedure TTreeViewPresenter.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  SetTreeView(FTreeView);
end;

procedure TTreeViewPresenter.SetItemsSource(const Value: TList<TObject>);
var
  LNotifyCollectionChanged: INotifyCollectionChanged;
  LCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  if FItemsSource <> Value then
  begin
    if Supports(FItemsSource, INotifyCollectionChanged, LNotifyCollectionChanged) then
    begin
      LCollectionChanged := LNotifyCollectionChanged.OnCollectionChanged;
      LCollectionChanged.Remove(DoSourceCollectionChanged);
    end;

    FItemsSource := Value;

    if Supports(FItemsSource, INotifyCollectionChanged, LNotifyCollectionChanged) then
    begin
      LCollectionChanged := LNotifyCollectionChanged.OnCollectionChanged;
      LCollectionChanged.Add(DoSourceCollectionChanged)
    end;
    ResetRootNodeCount();

    DoPropertyChanged('ItemsSource');
  end;
end;

procedure TTreeViewPresenter.SetItemTemplate(const Value: IDataTemplate);
begin
  FItemTemplate := Value;
  if Assigned(FTreeView) then
  begin
    InitColumns();
    ResetRootNodeCount();
  end;
end;

procedure TTreeViewPresenter.SetListMode(const Value: Boolean);
begin
  FListMode := Value;
  InitTreeOptions();
end;

procedure TTreeViewPresenter.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect := Value;
  InitTreeOptions();
end;

procedure TTreeViewPresenter.SetNodeItem(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Item: TObject);
begin
  if Assigned(Tree) and Assigned(Node) then
  begin
    PObject(Tree.GetNodeData(Node))^ := Item;
  end;
end;

procedure TTreeViewPresenter.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
  SetTreeView(FTreeView);
end;

procedure TTreeViewPresenter.SetSelectedItem(const Value: TObject);
begin
  FSelectedItems.Clear();
  FSelectedItems.Add(Value);
  SetSelectedItems(FSelectedItems);
end;

procedure TTreeViewPresenter.SetSelectedItems(const Value: TList<TObject>);
var
  LItem: TObject;
  LNode: PVirtualNode;
begin
  FTreeView.BeginUpdate();
  FTreeView.ClearSelection();
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LItem := GetNodeItem(FTreeView, LNode);
      if Assigned(LItem) and (Value.IndexOf(LItem) > -1) then
      begin
        FTreeView.Selected[LNode] := True;
      end;
      LNode := FTreeView.GetNext(LNode);
    end;
  end;
  FTreeView.EndUpdate();
end;

procedure TTreeViewPresenter.SetTreeView(const Value: TVirtualStringTree);
begin
  FTreeView := Value;
  if Assigned(FTreeView) then
  begin
    FTreeView.Images := FImageList;
    FTreeView.NodeDataSize := SizeOf(TObject);
    FTreeView.PopupMenu := FPopupMenu;

    InitColumns();
    InitTreeOptions();
    InitEvents();
    ResetRootNodeCount();
  end;
end;

procedure TTreeViewPresenter.UpdateCheckedItems;
var
  LItem: TObject;
  LNode: PVirtualNode;
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

procedure TTreeViewPresenter.UpdateExpandedItems;
var
  LItem: TObject;
  LNode: PVirtualNode;
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

procedure TTreeViewPresenter.UpdateSelectedItems;
var
  i: Integer;
  LItem: TObject;
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: TEvent<TPropertyChangedEvent>;
  LSelectedNodes: TNodeArray;
begin
  for LItem in FSelectedItems do
  begin
    if Supports(LItem, INotifyPropertyChanged, LNotifyPropertyChanged) then
    begin
      LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
      LPropertyChanged.Remove(DoCurrentItemPropertyChanged);
    end;
  end;

  FSelectedItems.Clear();
  LSelectedNodes := FTreeView.GetSortedSelection(False);

  for i := Low(LSelectedNodes) to High(LSelectedNodes) do
  begin
    LItem := GetNodeItem(FTreeView, LSelectedNodes[i]);
    if Assigned(LItem) then
    begin
      FSelectedItems.Add(LItem);
      if Supports(LItem, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Add(DoCurrentItemPropertyChanged);
      end;
    end;
  end;

  DoPropertyChanged('CurrentItem');
  DoPropertyChanged('SelectedItem');
  DoPropertyChanged('SelectedItems');
end;

end.
