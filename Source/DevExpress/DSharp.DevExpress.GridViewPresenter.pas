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

unit DSharp.DevExpress.GridViewPresenter;

interface

uses
  Classes,
  cxCustomData,
  cxGraphics,
  cxGridCustomTableView,
  cxGridCustomView,
  DSharp.Collections,
  DSharp.DevExpress.PresenterDataSource,
  DSharp.Windows.CustomPresenter;

type
  TGridViewPresenter = class(TCustomPresenter)
  private
    FDataSource: TPresenterDataSource;
    FGridView: TcxCustomGridView;

    procedure DoFocusedRecordChanged(Sender: TcxCustomGridTableView;
      PrevFocusedRecord, FocusedRecord: TcxCustomGridRecord;
      NewItemRecordFocusingChanged: Boolean);
    procedure SetGridView(const Value: TcxCustomGridView);
  protected
    function GetCurrentItem: TObject; override;
    procedure InitColumns; override;
    procedure InitEvents; override;
    procedure InitProperties; override;
    procedure SetCurrentItem(const Value: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Refresh; override;
  published
    property GridView: TcxCustomGridView read FGridView write SetGridView;
  end;

implementation

uses
  cxGridCardView,
  cxGridTableView,
  DSharp.Windows.ControlTemplates,
  SysUtils;

{ TGridViewPresenter }

constructor TGridViewPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FDataSource := TPresenterDataSource.Create(Self);
end;

destructor TGridViewPresenter.Destroy;
begin
  FDataSource.Free();
  inherited;
end;

procedure TGridViewPresenter.DoFocusedRecordChanged(
  Sender: TcxCustomGridTableView; PrevFocusedRecord,
  FocusedRecord: TcxCustomGridRecord; NewItemRecordFocusingChanged: Boolean);
begin
  if Assigned(FocusedRecord) then
  begin
    View.ItemIndex := FocusedRecord.RecordIndex;
  end
  else
  begin
    View.ItemIndex := -1;
  end;

  DoPropertyChanged('View');
end;

function TGridViewPresenter.GetCurrentItem: TObject;
begin
  if Assigned(FGridView) and (View.ItemIndex > -1) then
  begin
    Result := FDataSource.GetRecordHandleByIndex(View.ItemIndex);
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TGridViewPresenter.InitColumns;
var
  i: Integer;
  LTableView: TcxGridTableView;
  LCardView: TcxGridCardView;
begin
  if Assigned(FGridView) then
  begin
    if Assigned(ColumnDefinitions) then
    begin
      if FGridView is TcxGridTableView then
      begin
        LTableView := TcxGridTableView(FGridView);
        for i := 0 to Pred(ColumnDefinitions.Count) do
        begin
          if i < LTableView.ColumnCount then
          begin
            LTableView.Columns[i].Caption := ColumnDefinitions[i].Caption;
            LTableView.Columns[i].Width := ColumnDefinitions[i].Width;
          end;
        end;
      end else
      if FGridView is TcxGridCardView then
      begin
        LCardView := TcxGridCardView(FGridView);
        for i := 0 to Pred(ColumnDefinitions.Count) do
        begin
          if i < LCardView.RowCount then
          begin
            LCardView.Rows[i].Caption := ColumnDefinitions[i].Caption;
          end;
        end;
      end;
    end;
  end;
end;

procedure TGridViewPresenter.InitEvents;
begin
  if Assigned(FGridView) then
  begin
    FGridView.OnDblClick := DoDblClick;
    if FGridView is TcxCustomGridTableView then
    begin
      TcxCustomGridTableView(FGridView).OnFocusedRecordChanged := DoFocusedRecordChanged;
    end;
  end;
end;

procedure TGridViewPresenter.InitProperties;
begin
  if Assigned(FGridView) then
  begin
    FGridView.PopupMenu := PopupMenu;
    FGridView.DataController.CustomDataSource := FDataSource;
  end;
end;

procedure TGridViewPresenter.Refresh;
begin
  if Assigned(FGridView) and ([csLoading, csDesigning] * ComponentState = []) then
  begin
    FDataSource.DataChanged;
  end;
end;

procedure TGridViewPresenter.SetCurrentItem(const Value: TObject);
begin
  if Assigned(FGridView) then
  begin
    FDataSource.DataController.FocusedRecordIndex := FDataSource.GetRecordIndexByHandle(Value);
  end;
end;

procedure TGridViewPresenter.SetGridView(const Value: TcxCustomGridView);
begin
  FGridView := Value;
  InitControl();
end;

end.
