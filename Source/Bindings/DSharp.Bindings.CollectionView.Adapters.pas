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
  DSharp.Bindings.Notifications,
  DSharp.Collections,
  DSharp.Bindings.CollectionView;

type
  TCollectionViewStringsAdapter = class(TCollectionView)
  private
    FItems: TStrings;
    FOwner: TPersistent;
  protected
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction); override;
    function GetCurrentItem: TObject; override;
    function GetOwner: TPersistent; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure UpdateItems(AClearItems: Boolean = False); override;
  public
    constructor Create(AOwner: TPersistent; AItems: TStrings);
    destructor Destroy; override;

    procedure DoCurrentItemPropertyChanged(Sender: TObject;
      PropertyName: string; UpdateTrigger: TUpdateTrigger = utPropertyChanged);
  end;

implementation

uses
  DSharp.Core.Reflection,
  Rtti;

{ TCollectionViewStringsAdapter }

constructor TCollectionViewStringsAdapter.Create(AOwner: TPersistent; AItems: TStrings);
begin
  inherited Create();
  FItems := AItems;
  FOwner := AOwner;
end;

destructor TCollectionViewStringsAdapter.Destroy;
begin
  FItems := nil;
  SetItemsSource(nil);
  inherited;
end;

procedure TCollectionViewStringsAdapter.DoCurrentItemPropertyChanged(Sender: TObject;
  PropertyName: string; UpdateTrigger: TUpdateTrigger);
var
  LIndex: Integer;
begin
  LIndex := FItemIndex;
  if (LIndex > -1) and Assigned(FItemTemplate) then
  begin
    FItems[LIndex] := FItemTemplate.GetText(FItems.Objects[LIndex], -1);
  end;
end;

procedure TCollectionViewStringsAdapter.DoSourceCollectionChanged(Sender, Item: TObject;
  Action: TCollectionChangedAction);
var
  i: Integer;
begin
  case Action of
    caAdd:
    begin
      if not Assigned(FFilter) or FFilter(Item) then
      begin
        FItems.AddObject(ItemTemplate.GetText(Item, -1), Item);
      end;
    end;
    caRemove:
    begin
      i := FItems.IndexOfObject(Item);
      if i = FItemIndex then
      begin
        FItems.Delete(i);
        FItemIndex := -1;
        NotifyPropertyChanged(FOwner, Self, 'View');
      end
      else
      begin
        FItems.Delete(i);
      end;
    end;
  end;
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

function TCollectionViewStringsAdapter.GetOwner: TPersistent;
begin
  Result := FOwner;
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

procedure TCollectionViewStringsAdapter.UpdateItems(AClearItems: Boolean);
var
  LItem: TObject;
begin
  if Assigned(FItems) then
  begin
    if AClearItems then
    begin
      FItems.Clear;
    end;

    if Assigned(FItemsSource) then
    begin
      for LItem in FItemsSource do
      begin
        if not Assigned(FFilter) or FFilter(LItem) then
        begin
          if AClearItems then
          begin
            FItems.AddObject(ItemTemplate.GetText(LItem, -1), LItem);
          end
          else
          begin
            FItems[FItems.IndexOfObject(LItem)] := ItemTemplate.GetText(LItem, -1);
          end;
        end;
      end;
    end;
  end;
end;

end.
