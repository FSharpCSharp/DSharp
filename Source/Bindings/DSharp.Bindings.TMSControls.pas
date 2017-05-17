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

unit DSharp.Bindings.TMSControls;

interface

uses
  AdvGrid,
  Classes,
  DSharp.Bindings.Collections,
  DSharp.Bindings.CollectionView,
  DSharp.Bindings.Notifications;

type
  TAdvStringGrid = class(AdvGrid.TAdvStringGrid, INotifyPropertyChanged, ICollectionView)
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

implementation

uses
  DSharp.Bindings.CollectionView.TMSAdapters,
  DSharp.Core.Reflection;

{ TAdvStringGrid }

constructor TAdvStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyPropertyChanged := TNotifyPropertyChanged.Create(Self);
  FView := TCollectionViewAdvStringGridAdapter.Create(Self, Self);
end;

destructor TAdvStringGrid.Destroy;
begin
  FView.Free();
  inherited;
end;

function TAdvStringGrid.CanEditShow: Boolean;
begin
  Result := inherited and (FView.CurrentItem <> nil);
end;

function TAdvStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited;
  FView.ItemIndex := ARow - FixedRows;
  NotifyPropertyChanged.DoPropertyChanged('Selected');
end;

procedure TAdvStringGrid.SetEditText(ACol, ARow: Integer; const Value: string);
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

end.
