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

unit DSharp.DevExpress.TreeListPresenter;

interface

uses
  Classes,
  cxGraphics,
  cxTL,
  cxTLData,
  DSharp.Collections,
  DSharp.DevExpress.PresenterDataSource,
  DSharp.Windows.CustomPresenter,
  ImgList;

type
  TTreeListPresenter = class(TCustomPresenter)
  private
    FDataSource: TPresenterDataSource;
    FTreeList: TcxVirtualTreeList;

    procedure DoCustomDrawDataCell(Sender: TcxCustomTreeList;
      ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo;
      var ADone: Boolean);
    procedure DoGetNodeImageIndex(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
      var AIndex: TImageIndex);

    procedure SetTreeList(const Value: TcxVirtualTreeList);
  protected
    procedure InitColumns; override;
    procedure InitEvents; override;
    procedure InitProperties; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Refresh; override;
    property TreeList: TcxVirtualTreeList read FTreeList write SetTreeList;
  end;

implementation

uses
  DSharp.Core.DataTemplates,
  DSharp.Windows.ControlTemplates;

{ TTreeListPresenter }

constructor TTreeListPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FDataSource := TPresenterDataSource.Create(Self);
end;

destructor TTreeListPresenter.Destroy;
begin
  FDataSource.Free();
  inherited;
end;

procedure TTreeListPresenter.DoCustomDrawDataCell(Sender: TcxCustomTreeList;
  ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo;
  var ADone: Boolean);
var
  LItem: TObject;
  LItemTemplate: IControlTemplate;
begin
  LItem := TcxVirtualTreeListNode(AViewInfo.Node).RecordHandle;
  if Supports(GetItemTemplate(LItem), IControlTemplate, LItemTemplate) then
  begin
    ADone := LItemTemplate.CustomDraw(LItem, AViewInfo.Column.ItemIndex,
      ACanvas.Canvas, AViewInfo.VisibleRect, ImageList, dmAfterCellPaint);
  end;
end;

procedure TTreeListPresenter.DoGetNodeImageIndex(Sender: TcxCustomTreeList;
  ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType;
  var AIndex: TImageIndex);
var
  LItem: TObject;
  LItemTemplate: IDataTemplate;
begin
  AIndex := -1;

  LItem := TcxVirtualTreeListNode(ANode).RecordHandle;
  LItemTemplate := GetItemTemplate(LItem);
  if Assigned(LItemTemplate) then
  begin
    AIndex := LItemTemplate.GetImageIndex(LItem, 0);
  end;
end;

procedure TTreeListPresenter.InitColumns;
var
  i: Integer;
begin
  if Assigned(FTreeList) then
  begin
    if Assigned(ColumnDefinitions) then
    begin
      ColumnDefinitions.Clear();
      for i := 0 to Pred(FTreeList.ColumnCount) do
      begin
        with ColumnDefinitions.Add do
        begin
          Caption := FTreeList.Columns[i].Caption.Text;
          Width := FTreeList.Columns[i].Width;
        end;
      end;
    end;
  end;
end;

procedure TTreeListPresenter.InitEvents;
begin
  if Assigned(FTreeList) then
  begin
    FTreeList.OnCustomDrawDataCell := DoCustomDrawDataCell;
    FTreeList.OnGetNodeImageIndex := DoGetNodeImageIndex;
  end;
end;

procedure TTreeListPresenter.InitProperties;
begin
  if Assigned(FTreeList) then
  begin
    FTreeList.Images := ImageList;
    FTreeList.PopupMenu := PopupMenu;
    FTreeList.OptionsData.SmartLoad := True;
    FTreeList.CustomDataSource := FDataSource;
  end;
end;

procedure TTreeListPresenter.Refresh;
begin
  if Assigned(FTreeList) then
  begin
    FDataSource.DataChanged;
  end;
end;

procedure TTreeListPresenter.SetTreeList(const Value: TcxVirtualTreeList);
begin
  FTreeList := Value;
  InitControl();
end;

end.
