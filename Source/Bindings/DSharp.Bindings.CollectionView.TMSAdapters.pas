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

unit DSharp.Bindings.CollectionView.TMSAdapters;

interface

uses
  AdvGrid,
  DSharp.Bindings.CollectionView.VCLAdapters;

type
  TCollectionViewAdvStringGridAdapter = class(TCollectionViewStringGridAdapter)
  private
    function GetGrid: TAdvStringGrid;
  protected
    procedure ClearRow(AIndex: NativeInt); override;
    function FindDisplayItem(AItem: TObject): NativeInt; override;
    function GetCurrentItem: TObject; override;
    procedure UpdateDisplayItem(AIndex: NativeInt; AItem: TObject); override;

    property Grid: TAdvStringGrid read GetGrid;
  end;

implementation

{ TCollectionViewAdvStringGridAdapter }

procedure TCollectionViewAdvStringGridAdapter.ClearRow(AIndex: NativeInt);
begin
  Grid.Rows[AIndex + Grid.FixedRows].Text := '';
  Grid.Objects[0, AIndex + Grid.FixedRows] := nil;
end;

function TCollectionViewAdvStringGridAdapter.FindDisplayItem(
  AItem: TObject): NativeInt;
var
  i: Integer;
begin
  Result := -1;
  for i := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    if Grid.Objects[0, i] = AItem then
    begin
      Result := i;
    end;
  end;
  if Result > -1 then
  begin
    Result := Result - Grid.FixedRows;
  end;
end;

function TCollectionViewAdvStringGridAdapter.GetCurrentItem: TObject;
begin
  if Assigned(Grid) and (FItemIndex > -1) then
  begin
    Result := Grid.Objects[0, FItemIndex + Grid.FixedRows];
  end
  else
  begin
    Result := nil;
  end;
end;

function TCollectionViewAdvStringGridAdapter.GetGrid: TAdvStringGrid;
begin
  Result := TAdvStringGrid(inherited Grid);
end;

procedure TCollectionViewAdvStringGridAdapter.UpdateDisplayItem(
  AIndex: NativeInt; AItem: TObject);
var
  i: Integer;
begin
  Grid.Objects[0, AIndex + Grid.FixedRows] := AItem;
  for i := Grid.FixedCols to Grid.ColCount - 1 do
  begin
    Grid.Cells[i, AIndex + Grid.FixedRows] := ItemTemplate.GetText(AItem, i - Grid.FixedCols);
  end;
end;

end.
