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

unit DSharp.Bindings.CollectionView.Adapters;

interface

uses
  Classes,
  ComCtrls,
  DSharp.Collections,
  DSharp.Bindings.CollectionView,
  DSharp.Bindings.Notifications;

type
  TCollectionViewAdapter = class abstract(TCollectionView)
  private
    FOwner: TPersistent;
  protected
    function AddDisplayItem: Integer; virtual; abstract;
    procedure ClearDisplayItems; virtual; abstract;
    function FindDisplayItem(AItem: TObject): Integer; virtual; abstract;
    function GetDisplayItemsCount: Integer; virtual; abstract;
    procedure RemoveDisplayItem(AIndex: Integer); virtual; abstract;
    procedure UpdateDisplayItem(AIndex: Integer; AItem: TObject); virtual; abstract;

    procedure DoItemPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged); override;
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction); override;
    function GetOwner: TPersistent; override;
    procedure UpdateItemIndex(ACurrentItem: TObject);
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent);
  end;

  TCollectionViewStringsAdapter = class(TCollectionViewAdapter)
  private
    FItems: TStrings;
  protected
    function AddDisplayItem: Integer; override;
    procedure ClearDisplayItems; override;
    function FindDisplayItem(AItem: TObject): Integer; override;
    function GetDisplayItemsCount: Integer; override;
    procedure RemoveDisplayItem(AIndex: Integer); override;
    procedure UpdateDisplayItem(AIndex: Integer; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; AItems: TStrings);
    destructor Destroy; override;
  end;

  TCollectionViewListItemsAdapter = class(TCollectionViewAdapter)
  private
    FItems: TListItems;
    FColumns: TListColumns;
  protected
    function AddDisplayItem: Integer; override;
    procedure ClearDisplayItems; override;
    function GetDisplayItemsCount: Integer; override;
    function FindDisplayItem(AItem: TObject): Integer; override;
    procedure RemoveDisplayItem(AIndex: Integer); override;
    procedure UpdateDisplayItem(AIndex: Integer; AItem: TObject); override;

    function GetCurrentItem: TObject; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; AItems: TListItems;
      AColumns: TListColumns);
    destructor Destroy; override;
  end;

implementation

uses
  DSharp.Core.Reflection,
  Rtti;

{ TCollectionViewAdapter }

constructor TCollectionViewAdapter.Create(AOwner: TPersistent);
begin
  inherited Create();
  FOwner := AOwner;
end;

procedure TCollectionViewAdapter.DoItemPropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
var
  LIndex: Integer;
begin
  LIndex := FindDisplayItem(ASender);

  if not Assigned(FFilter) or FFilter(ASender) then
  begin
    if LIndex = -1 then
    begin
      LIndex := AddDisplayItem();
    end;

    UpdateDisplayItem(LIndex, ASender);
  end
  else
  begin
    if LIndex > -1 then
    begin
      RemoveDisplayItem(LIndex);
    end;
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewAdapter.DoSourceCollectionChanged(Sender,
  Item: TObject; Action: TCollectionChangedAction);
var
  LIndex: Integer;
begin
  inherited;

  case Action of
    caAdd:
    begin
      if not Assigned(FFilter) or FFilter(Item) then
      begin
        LIndex := AddDisplayItem();
        UpdateDisplayItem(LIndex, Item);
      end;
    end;
    caRemove:
    begin
      LIndex := FindDisplayItem(Item);
      RemoveDisplayItem(LIndex);
    end;
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

function TCollectionViewAdapter.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TCollectionViewAdapter.UpdateItemIndex(ACurrentItem: TObject);
begin
  if Assigned(ACurrentItem) then
  begin
    ItemIndex := FindDisplayItem(ACurrentItem);
  end
  else
  begin
    while not (FItemIndex < GetDisplayItemsCount) do
    begin
      Dec(FItemIndex);
    end;
    ItemIndex := FItemIndex;
  end;
end;

procedure TCollectionViewAdapter.UpdateItems(AClearItems: Boolean);
var
  LCurrentItem: TObject;
  LIndex: Integer;
  LItem: TObject;
begin
  LCurrentItem := CurrentItem;

  if AClearItems then
  begin
    ClearDisplayItems;
  end;

  if Assigned(FItemsSource) then
  begin
    for LItem in FItemsSource do
    begin
      LIndex := FindDisplayItem(LItem);
      if not Assigned(FFilter) or FFilter(LItem) then
      begin
        if LIndex = -1 then
        begin
          LIndex := AddDisplayItem();
        end;

        UpdateDisplayItem(LIndex, LItem);
      end;
    end;
  end;

  UpdateItemIndex(LCurrentItem);

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

{ TCollectionViewListItemsAdapter }

constructor TCollectionViewListItemsAdapter.Create(AOwner: TPersistent;
  AItems: TListItems; AColumns: TListColumns);
begin
  inherited Create(AOwner);
  FItems := AItems;
  FColumns := AColumns;
end;

destructor TCollectionViewListItemsAdapter.Destroy;
begin
  FColumns := nil;
  FItems := nil;
  SetItemsSource(nil);
  inherited;
end;

procedure TCollectionViewListItemsAdapter.ClearDisplayItems;
begin
  FItems.Clear();
end;

function TCollectionViewListItemsAdapter.AddDisplayItem: Integer;
var
  LListItem: TListItem;
begin
  LListItem := FItems.Add;
  Result := LListItem.Index;
end;

function TCollectionViewListItemsAdapter.GetCurrentItem: TObject;
begin
  if ItemIndex > -1 then
  begin
    Result := FItems[ItemIndex].Data;
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewListItemsAdapter.GetDisplayItemsCount: Integer;
begin
  Result := FItems.Count;
end;

function TCollectionViewListItemsAdapter.FindDisplayItem(AItem: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Pred(FItems.Count) do
  begin
    if FItems[i].Data = AItem then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TCollectionViewListItemsAdapter.RemoveDisplayItem(AIndex: Integer);
begin
  if AIndex = ItemIndex then
  begin
    FItems.Delete(AIndex);
    UpdateItemIndex(nil);
    NotifyPropertyChanged(FOwner, Self, 'View');
  end
  else
  begin
    FItems.Delete(AIndex);
  end;
end;

procedure TCollectionViewListItemsAdapter.UpdateDisplayItem(AIndex: Integer;
  AItem: TObject);
var
  i: Integer;
  LListItem: TListItem;
begin
  LListItem := FItems[AIndex];
  LListItem.Data := AItem;
  LListItem.SubItems.Clear();
  for i := 0 to Pred(FColumns.Count) do
  begin
    if i = 0 then
    begin
      LListItem.Caption := ItemTemplate.GetText(AItem, i);
    end
    else
    begin
      LListItem.SubItems.Add(ItemTemplate.GetText(AItem, i));
    end;
  end;
end;

procedure TCollectionViewListItemsAdapter.SetItemIndex(const Value: Integer);
var
  LProperty: TRttiProperty;
begin
  inherited;

  if FOwner.TryGetProperty('ItemIndex', LProperty) then
  begin
    LProperty.SetValue(FOwner, Value);
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewListItemsAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FItems) then
  begin
    inherited;
  end;
end;

{ TCollectionViewStringsAdapter }

constructor TCollectionViewStringsAdapter.Create(AOwner: TPersistent; AItems: TStrings);
begin
  inherited Create(AOwner);
  FItems := AItems;
  FOwner := AOwner;
end;

destructor TCollectionViewStringsAdapter.Destroy;
begin
  FItems := nil;
  SetItemsSource(nil);
  inherited;
end;

function TCollectionViewStringsAdapter.AddDisplayItem: Integer;
begin
  Result := FItems.AddObject('', nil);
end;

procedure TCollectionViewStringsAdapter.ClearDisplayItems;
begin
  FItems.Clear();
end;

function TCollectionViewStringsAdapter.FindDisplayItem(AItem: TObject): Integer;
begin
  Result := FItems.IndexOfObject(AItem);
end;

function TCollectionViewStringsAdapter.GetCurrentItem: TObject;
begin
  if FItemIndex > -1 then
  begin
    Result := FItems.Objects[FItemIndex];
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewStringsAdapter.GetDisplayItemsCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TCollectionViewStringsAdapter.RemoveDisplayItem(AIndex: Integer);
begin
  if AIndex = ItemIndex then
  begin
    FItems.Delete(AIndex);
    UpdateItemIndex(nil);
    NotifyPropertyChanged(FOwner, Self, 'View');
  end
  else
  begin
    FItems.Delete(AIndex);
  end;
end;

procedure TCollectionViewStringsAdapter.SetItemIndex(const Value: Integer);
var
  LProperty: TRttiProperty;
begin
  inherited;

  if FOwner.TryGetProperty('ItemIndex', LProperty) then
  begin
    LProperty.SetValue(FOwner, Value);
  end;

  NotifyPropertyChanged(FOwner, Self, 'View');
end;

procedure TCollectionViewStringsAdapter.UpdateDisplayItem(AIndex: Integer;
  AItem: TObject);
begin
  FItems[AIndex] := ItemTemplate.GetText(AItem, -1);
  FItems.Objects[AIndex] := AItem;
end;

procedure TCollectionViewStringsAdapter.UpdateItems(AClearItems: Boolean);
begin
  if Assigned(FItems) then
  begin
    inherited;
  end;
end;

end.
