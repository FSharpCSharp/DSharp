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

unit DSharp.Bindings.CollectionView;

interface

uses
  Classes,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Core.PropertyChangedBase,
  DSharp.Core.Events,
  SysUtils;

type
  TCollectionView = class(TPropertyChangedBase, ICollectionView, ICollectionViewNavigation)
  private
    FUpdateCount: Integer;
    function GetUpdating: Boolean;
  protected
    FFilter: TPredicate<TObject>;
    FItemIndex: NativeInt;
    FItemsSource: IList<TObject>;
    FItemTemplate: IDataTemplate;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;

    procedure DoItemPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged); virtual;
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction); virtual;
    function GetCanMoveCurrentToNext: Boolean; virtual;
    function GetCanMoveCurrentToPrevious: Boolean; virtual;
    function GetCurrentItem: TObject; virtual;
    function GetFilter: TPredicate<TObject>; virtual;
    function GetItemsSource: IList<TObject>; virtual;
    function GetItemTemplate: IDataTemplate; virtual;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    procedure SetCurrentItem(const Value: TObject); virtual;
    procedure SetFilter(const Value: TPredicate<TObject>); virtual;
    procedure SetItemIndex(const Value: NativeInt); virtual;
    procedure SetItemsSource(const Value: IList<TObject>); virtual;
    procedure SetItemTemplate(const Value: IDataTemplate); virtual;
    procedure UpdateItems(AClearItems: Boolean = False); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure MoveCurrentToFirst; virtual;
    procedure MoveCurrentToLast; virtual;
    procedure MoveCurrentToNext; virtual;
    procedure MoveCurrentToPrevious; virtual;

    property CanMoveCurrentToNext: Boolean read GetCanMoveCurrentToNext;
    property CanMoveCurrentToPrevious: Boolean read GetCanMoveCurrentToPrevious;
    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property Filter: TPredicate<TObject> read GetFilter write SetFilter;
    property ItemIndex: NativeInt read FItemIndex write SetItemIndex;
    property ItemsSource: IList<TObject> read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent>
      read GetOnCollectionChanged;
    property Updating: Boolean read GetUpdating;
  end;

implementation

uses
  DSharp.Core.DataTemplates.Default,
  DSharp.Core.Utils;

{ TCollectionView }

constructor TCollectionView.Create;
begin
  inherited;
  FItemIndex := -1;
end;

destructor TCollectionView.Destroy;
begin
  ItemsSource := nil;
  inherited;
end;

procedure TCollectionView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCollectionView.DoItemPropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
begin

end;

procedure TCollectionView.DoSourceCollectionChanged(Sender, Item: TObject;
  Action: TCollectionChangedAction);
begin
  case Action of
    caReplace:
    begin
      DoItemPropertyChanged(Item, '');
    end;
  end;
end;

procedure TCollectionView.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
  end;
end;

function TCollectionView.GetCanMoveCurrentToNext: Boolean;
begin
  // TODO: Implement filter support
  Result := not Assigned(FFilter) and Assigned(FItemsSource)
    and (FItemIndex < Pred(FItemsSource.Count));
end;

function TCollectionView.GetCanMoveCurrentToPrevious: Boolean;
begin
  // TODO: Implement filter support
  Result := not Assigned(FFilter) and Assigned(FItemsSource)
    and (FItemIndex > 0);
end;

function TCollectionView.GetCurrentItem: TObject;
begin
  Result := nil;
end;

function TCollectionView.GetFilter: TPredicate<TObject>;
begin
  Result := FFilter;
end;

function TCollectionView.GetItemsSource: IList<TObject>;
begin
  Result := FItemsSource;
end;

function TCollectionView.GetItemTemplate: IDataTemplate;
begin
  if not Assigned(FItemTemplate) then
  begin
    FItemTemplate := TDefaultDataTemplate.Create();
  end;
  Result := FItemTemplate;
end;

function TCollectionView.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TCollectionView.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TCollectionView.MoveCurrentToFirst;
begin
  if not Assigned(FFilter)  then
  begin
    if CanMoveCurrentToPrevious xor (FItemIndex = -1)
      and Assigned(FItemsSource) and (FItemsSource.Count > 0) then
    begin
      ItemIndex := 0;
    end;
  end;
end;

procedure TCollectionView.MoveCurrentToLast;
begin
  if not Assigned(FFilter) then
  begin
    if CanMoveCurrentToNext then
    begin
      ItemIndex := Pred(FItemsSource.Count);
    end;
  end;
end;

procedure TCollectionView.MoveCurrentToNext;
begin
  if not Assigned(FFilter) then
  begin
    if CanMoveCurrentToNext then
    begin
      ItemIndex := ItemIndex + 1;
    end;
  end;
end;

procedure TCollectionView.MoveCurrentToPrevious;
begin
  if not Assigned(FFilter) then
  begin
    if CanMoveCurrentToPrevious then
    begin
      ItemIndex := ItemIndex - 1;
    end;
  end;
end;

procedure TCollectionView.SetCurrentItem(const Value: TObject);
begin
  if Assigned(FItemsSource) then
  begin
    ItemIndex := FItemsSource.IndexOf(Value);
  end;
end;

procedure TCollectionView.SetFilter(const Value: TPredicate<TObject>);
begin
  FFilter := Value;
  UpdateItems(True);
end;

procedure TCollectionView.SetItemIndex(const Value: NativeInt);
begin
  FItemIndex := Value;

  DoPropertyChanged('CurrentItem');
  DoPropertyChanged('ItemIndex');
  DoPropertyChanged('CanMoveCurrentToNext');
  DoPropertyChanged('CanMoveCurrentToPrevious');
end;

procedure TCollectionView.SetItemsSource(const Value: IList<TObject>);
var
  LCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  if FItemsSource <> Value then
  begin
    if Assigned(FItemsSource) then
    begin
      LCollectionChanged := TEvent<TCollectionChangedEvent>(FItemsSource.OnCollectionChanged);
      LCollectionChanged.Remove(DoSourceCollectionChanged);
    end;

    FItemsSource := Value;

    if Assigned(FItemsSource) then
    begin
      LCollectionChanged := TEvent<TCollectionChangedEvent>(FItemsSource.OnCollectionChanged);
      LCollectionChanged.Add(DoSourceCollectionChanged);
    end;
    UpdateItems(True);

    DoPropertyChanged('ItemsSource');
  end;
end;

procedure TCollectionView.SetItemTemplate(const Value: IDataTemplate);
begin
  FItemTemplate := Value;
  UpdateItems(False);
end;

procedure TCollectionView.UpdateItems(AClearItems: Boolean);
begin
  // implemented by child classes
end;

end.
