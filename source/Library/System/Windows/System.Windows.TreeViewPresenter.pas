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

unit System.Windows.TreeViewPresenter;

interface

uses
  ActiveX,
  Classes,
  Controls,
  Generics.Collections,
  Menus,
  System.Bindings,
  System.Bindings.Collections,
  System.Data.Templates,
  System.Events,
  System.Windows.ColumnDefinitions,
  SysUtils,
  Types,
  VirtualTrees;

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Item: TObject;
  end;

  TCompareEvent = procedure(Sender: TObject; Item1, Item2: TObject;
    ColumnIndex: Integer; var Result: Integer) of object;
  TDragBeginEvent = procedure(Sender: TObject; var AllowDrag: Boolean) of object;
  TDragOverEvent = procedure(Sender: TObject; TargetItem: TObject;
    var AllowDrop: Boolean) of object;
  TDragDropEvent = procedure(Sender: TObject; TargetItem: TObject;
    DragOperation: TDragOperation) of object;

  TTreeViewPresenter = class(TComponent, ICollectionView, INotifyPropertyChanged)
  private
    FCheckedItems: TList<TObject>;
    FCheckSupport: Boolean;
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
    FUseRtti: Boolean;

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
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionNotification);

    function GetCheckedItems: TList<TObject>;
    function GetCurrentItem: TObject;
    function GetExpandedItems: TList<TObject>;
    function GetFilter: TPredicate<TObject>;
    function GetItemsSource: TList<TObject>;
    function GetItemTemplate: IDataTemplate; overload;
    function GetItemTemplate(const Item: TObject): IDataTemplate; overload;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetSelectedItem: TObject;
    function GetSelectedItems: TList<TObject>;

    procedure InitColumns;
    procedure InitEvents;
    procedure InitTreeOptions;

    procedure ResetRootNodeCount;

    procedure SetCheckSupport(const Value: Boolean);
    procedure SetColumnDefinitions(const Value: TColumnDefinitions);
    procedure SetCurrentItem(const Value: TObject);
    procedure SetExpandedItems(const Value: TList<TObject>);
    procedure SetFilter(const Value: TPredicate<TObject>);
    procedure SetImageList(const Value: TImageList);
    procedure SetItemsSource(const Value: TList<TObject>);
    procedure SetItemTemplate(const Value: IDataTemplate);
    procedure SetListMode(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetSelectedItem(const Value: TObject);
    procedure SetSelectedItems(const Value: TList<TObject>);
    procedure SetTreeView(const Value: TVirtualStringTree);

    procedure UpdateCheckedItems;
    procedure UpdateExpandedItems;
    procedure UpdateSelectedItems;
  protected
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
    property CheckSupport: Boolean read FCheckSupport write SetCheckSupport default False;
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
    property UseRtti: Boolean read FUseRtti write FUseRtti default False;
  end;

implementation

uses
  System.Data.Templates.Rtti;

const
  CDefaultCellRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{ TTreeViewPresenter }

constructor TTreeViewPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FCheckedItems := TList<TObject>.Create();
  FExpandedItems := TList<TObject>.Create();
  FSelectedItems := TList<TObject>.Create();
  FOnCollectionChanged.Add(DoSourceCollectionChanged);

  FColumnDefinitions := TColumnDefinitions.Create(Self);
end;

destructor TTreeViewPresenter.Destroy;
begin
  FCheckedItems.Free();
  FExpandedItems.Free();
  FSelectedItems.Free();
  inherited;
end;

procedure TTreeViewPresenter.DeleteItems(Items: TList<TObject>);
var
  LNode: PVirtualNode;
  LNodeData: PNodeData;
begin
  if Assigned(Items) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LNodeData := FTreeView.GetNodeData(LNode);
      if Assigned(LNodeData) and (Items.IndexOf(LNodeData.Item) > -1) then
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
  LNodeData: PNodeData;
  LItemTemplate: IDataTemplate;
begin
  LNodeData := Sender.GetNodeData(Node);
  LItemTemplate := GetItemTemplate(LNodeData.Item);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LNodeData.Item, Column, TargetCanvas, CellRect, FImageList, dmAfterCellPaint);
  end;
end;

procedure TTreeViewPresenter.DoBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  LNodeData: PNodeData;
  LItemTemplate: IDataTemplate;
begin
  LNodeData := Sender.GetNodeData(Node);
  LItemTemplate := GetItemTemplate(LNodeData.Item);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LNodeData.Item, Column, TargetCanvas, CellRect, FImageList, dmBeforeCellPaint);
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
  LNodeData1, LNodeData2: PNodeData;
  LItemTemplate1, LItemTemplate2: IDataTemplate;
begin
  LNodeData1 := Sender.GetNodeData(Node1);
  LNodeData2 := Sender.GetNodeData(Node2);
  LItemTemplate1 := GetItemTemplate(LNodeData1.Item);
  LItemTemplate2 := GetItemTemplate(LNodeData2.Item);

  if Assigned(FOnCompare) then
  begin
    FOnCompare(Self, LNodeData1.Item, LNodeData2.Item, Column, Result);
  end
  else
  begin
    // Using item template to sort
    if Assigned(LItemTemplate1) and Assigned(LItemTemplate2) then
    begin
      Result := CompareStr(LItemTemplate1.GetText(LNodeData1.Item, Column),
        LItemTemplate2.GetText(LNodeData2.Item, Column));
    end;
  end;
end;

procedure TTreeViewPresenter.DoCurrentItemPropertyChanged(Sender: TObject;
  PropertyName: string; UpdateTrigger: TUpdateTrigger);
var
  LNode: PVirtualNode;
begin
  if Assigned(FTreeView) and (FTreeView.SelectedCount > 0) then
  begin
    for LNode in FTreeView.GetSortedSelection(True) do
    begin
      FTreeView.InvalidateNode(LNode);
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

  if FListMode and (hiOnNormalIcon in LHitInfo.HitPositions) then
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
  LNode: PVirtualNode;
  LNodeData: PNodeData;
  LSelectedNodes: TNodeArray;
begin
  LNode := Sender.DropTargetNode;
  LNodeData := Sender.GetNodeData(LNode);
  if Assigned(LNodeData) and Assigned(FOnDragDrop) then
  begin
    LSelectedNodes := Sender.GetSortedSelection(False);
    if ssCtrl in Shift then
    begin
      FOnDragDrop(Sender, LNodeData.Item, doCopy);
      Sender.ReinitNode(LNode, True);
    end
    else
    begin
      FOnDragDrop(Sender, LNodeData.Item, doMove);
      for i := Low(LSelectedNodes) to High(LSelectedNodes) do
      begin
        FTreeView.MoveTo(LSelectedNodes[i], LNode, amAddChildLast, False);
      end;
    end;
  end;
end;

procedure TTreeViewPresenter.DoDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  LNode: PVirtualNode;
  LNodeData: PNodeData;
begin
  LNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  LNodeData := Sender.GetNodeData(LNode);
  if Assigned(LNodeData) and Assigned(FOnDragOver) then
  begin
    FOnDragOver(Sender, LNodeData.Item, Accept);
  end;
end;

procedure TTreeViewPresenter.DoFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  LNodeData: PNodeData;
begin
  LNodeData := Sender.GetNodeData(Node);
  if Assigned(LNodeData) then
  begin
    // nothing to do here yet
  end;
end;

procedure TTreeViewPresenter.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  LNodeData: PNodeData;
  LItemTemplate: IDataTemplate;
begin
  LNodeData := Sender.GetNodeData(Node);
  LItemTemplate := GetItemTemplate(LNodeData.Item);
  if Assigned(LItemTemplate) then
  begin
    ImageIndex := LItemTemplate.GetImageIndex(LNodeData.Item, Column);
  end;
end;

procedure TTreeViewPresenter.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LNodeData: PNodeData;
  LItemTemplate: IDataTemplate;
begin
  LNodeData := Sender.GetNodeData(Node);
  LItemTemplate := GetItemTemplate(LNodeData.Item);
  if Assigned(LItemTemplate) then
  begin
    CellText := LItemTemplate.GetText(LNodeData.Item, Column);
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
  LNodeData: PNodeData;
  LParentNodeData: PNodeData;
  LItemTemplate: IDataTemplate;
begin
  Node.CheckType := ctCheckBox;
  LNodeData := Sender.GetNodeData(Node);

  if Assigned(ParentNode) then
  begin
    LParentNodeData := Sender.GetNodeData(ParentNode);
    LItemTemplate := GetItemTemplate(LParentNodeData.Item);
    LNodeData.Item := LItemTemplate.GetItem(LParentNodeData.Item, Node.Index);
  end
  else
  begin
    LNodeData.Item := FItemsSource[Node.Index];
  end;

  LItemTemplate := GetItemTemplate(LNodeData.Item);
  if Assigned(LItemTemplate) then
  begin
    Sender.ChildCount[Node] := LItemTemplate.GetItemCount(LNodeData.Item);
  end
  else
  begin
    Sender.ChildCount[Node] := 0;
  end;

  if Assigned(FFilter) then
  begin
    Sender.IsFiltered[Node] := not FFilter(LNodeData.Item);
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

procedure TTreeViewPresenter.DoPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  LNodeData: PNodeData;
  LItemTemplate: IDataTemplate;
begin
  LNodeData := Sender.GetNodeData(Node);
  LItemTemplate := GetItemTemplate(LNodeData.Item);
  if Assigned(LItemTemplate) then
  begin
    LItemTemplate.CustomDraw(LNodeData.Item, Column, TargetCanvas, CDefaultCellRect, FImageList, dmPaintText);
  end;
end;

procedure TTreeViewPresenter.DoSourceCollectionChanged(Sender: TObject;
  Item: TObject; Action: TCollectionNotification);
begin
  ResetRootNodeCount();
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
  end
  else
  begin
    if FUseRtti then
    begin
      FItemTemplate := TRttiDataTemplate.Create(FColumnDefinitions);
      Result := FItemTemplate;
    end;
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
    FTreeView.OnMouseDown := DoMouseDown;
    FTreeView.OnPaintText := DoPaintText;
  end;
end;

procedure TTreeViewPresenter.InitTreeOptions;
begin
  if Assigned(FTreeView) and not (csDesigning in ComponentState) then
  begin
    if FCheckSupport then
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

procedure TTreeViewPresenter.SetCheckSupport(const Value: Boolean);
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
  LNode: PVirtualNode;
  LNodeData: PNodeData;
begin
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LNodeData := FTreeView.GetNodeData(LNode);
      if Assigned(LNodeData) and (Value.IndexOf(LNodeData.Item) > -1) then
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
  end;
end;

procedure TTreeViewPresenter.SetItemTemplate(const Value: IDataTemplate);
begin
  FItemTemplate := Value;
  if Assigned(FTreeView) then
  begin
    InitColumns();
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
  LNode: PVirtualNode;
  LNodeData: PNodeData;
begin
  FTreeView.BeginUpdate();
  FTreeView.ClearSelection();
  if Assigned(Value) then
  begin
    LNode := FTreeView.GetFirst();
    while Assigned(LNode) do
    begin
      LNodeData := FTreeView.GetNodeData(LNode);
      if Assigned(LNodeData) and (Value.IndexOf(LNodeData.Item) > -1) then
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
    FTreeView.NodeDataSize := SizeOf(TNodeData);
    FTreeView.PopupMenu := FPopupMenu;

    InitColumns();
    InitTreeOptions();
    InitEvents();
    ResetRootNodeCount();
  end;
end;

procedure TTreeViewPresenter.UpdateCheckedItems;
var
  LNode: PVirtualNode;
  LNodeData: PNodeData;
begin
  FCheckedItems.Clear();
  LNode := FTreeView.GetFirstChecked();
  while Assigned(LNode) do
  begin
    LNodeData := FTreeView.GetNodeData(LNode);
    if Assigned(LNodeData) then
    begin
      FCheckedItems.Add(LNodeData.Item);
    end;
    LNode := FTreeView.GetNextChecked(LNode);
  end;
end;

procedure TTreeViewPresenter.UpdateExpandedItems;
var
  LNode: PVirtualNode;
  LNodeData: PNodeData;
begin
  FExpandedItems.Clear();
  LNode := FTreeView.GetFirst();
  while Assigned(LNode) do
  begin
    LNodeData := FTreeView.GetNodeData(LNode);
    if Assigned(LNodeData) and FTreeView.Expanded[LNode] then
    begin
      FExpandedItems.Add(LNodeData.Item);
    end;
    LNode := FTreeView.GetNext(LNode);
  end;
end;

procedure TTreeViewPresenter.UpdateSelectedItems;
var
  i: Integer;
  LNodeData: PNodeData;
  LSelectedNodes: TNodeArray;
  LItem: TObject;
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: TEvent<TPropertyChangedEvent>;
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
    LNodeData := FTreeView.GetNodeData(LSelectedNodes[i]);
    if Assigned(LNodeData) then
    begin
      FSelectedItems.Add(LNodeData.Item);
      if Supports(LNodeData.Item, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
        LPropertyChanged.Add(DoCurrentItemPropertyChanged);
      end;
    end;
  end;
  FOnPropertyChanged.Invoke(Self, 'CurrentItem');
  FOnPropertyChanged.Invoke(Self, 'SelectedItem');
  FOnPropertyChanged.Invoke(Self, 'SelectedItems');
end;

end.
