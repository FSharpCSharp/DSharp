(*
  Copyright (c) 2012, Stefan Glienke
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

unit DSharp.Windows.CustomPresenter.DataSetAdapter;

interface

uses
  DB,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Windows.ColumnDefinitions,
  Rtti;

type
  TDataSetBookmark = class
    FBookmark: TBookmark;
  public
    constructor Create(ABookmark: TBookmark);
  end;

  TDataSetList = class(TList<TDataSetBookmark>, IList)
  private
    FDataSet: TDataSet;
    FNotify: Boolean;
    procedure DoAfterPost(DataSet: TDataSet);
  protected
    procedure Notify(const Value: TDataSetBookmark;
      const Action: TCollectionChangedAction); override;
  public
    constructor Create(ADataSet: TDataSet);
    destructor Destroy; override;
  end;

  TDataSetColumnDefinitions = class(TColumnDefinitions)
  public
    constructor Create(ADataSet: TDataSet); reintroduce;
  end;

  TDataSetTemplate = class(TDataTemplate)
  private
    FDataSet: TDataSet;
  public
    constructor Create(ADataSet: TDataSet);

    function GetItem(const Item: TObject;
      const Index: Integer): TObject; override;
    function GetItemCount(const Item: TObject): Integer; override;

    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; override;
    procedure SetText(const Item: TObject;
      const ColumnIndex: Integer; const Value: string); override;

    function CompareItems(const Item1, Item2: TObject;
      const ColumnIndex: Integer): Integer; override;

    function GetTemplateDataClass: TClass; override;
  end;

implementation

uses
  SysUtils;

{ TDataSetBookmark }

constructor TDataSetBookmark.Create(ABookmark: TBookmark);
begin
  FBookmark := ABookmark;
end;

{ TDataSetList }

constructor TDataSetList.Create(ADataSet: TDataSet);
begin
  inherited Create;

  FDataSet := ADataSet;
  FDataSet.AfterPost := DoAfterPost;
  FNotify := False;

  SetCapacity(FDataSet.RecordCount);
  FDataSet.First;
  while not FDataSet.Eof do
  begin
    Add(TDataSetBookmark.Create(FDataSet.GetBookmark));
    FDataSet.Next;
  end;

  FNotify := True;
end;

destructor TDataSetList.Destroy;
begin
  FNotify := False;
  inherited;
end;

procedure TDataSetList.DoAfterPost(DataSet: TDataSet);
begin
  if DataSet.RecNo > Count then
    Add(TDataSetBookmark.Create(DataSet.GetBookmark))
  else
    Items[DataSet.RecNo - 1].FBookmark := FDataSet.GetBookmark;
end;

procedure TDataSetList.Notify(const Value: TDataSetBookmark;
  const Action: TCollectionChangedAction);
begin
  if FNotify then
  begin
    inherited;

    if Action = caRemove then
    begin
      FDataSet.GotoBookmark(Value.FBookmark);
      FDataSet.Delete;
    end;
  end;
end;

{ TDataSetTemplate }

function TDataSetTemplate.CompareItems(const Item1, Item2: TObject;
  const ColumnIndex: Integer): Integer;
begin
  Result := CompareText(GetText(Item1, ColumnIndex), GetText(Item2, ColumnIndex));
end;

constructor TDataSetTemplate.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
end;

function TDataSetTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Item is TDataSetList then
    Result := TDataSetList(Item).Items[Index]
  else
    Result := nil;
end;

function TDataSetTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Item is TDataSetList then
    Result := TDataSetList(Item).Count
  else
    Result := 0;
end;

function TDataSetTemplate.GetTemplateDataClass: TClass;
begin
  Result := TDataSetBookmark;
end;

function TDataSetTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  FDataSet.GotoBookmark(TDataSetBookmark(Item).FBookmark);

  if ColumnIndex > -1 then
    Result := FDataSet.Fields[ColumnIndex].Value
  else
    Result := FDataSet.Fields[0].Value;
end;

procedure TDataSetTemplate.SetText(const Item: TObject;
  const ColumnIndex: Integer; const Value: string);
begin
  FDataSet.GotoBookmark(TDataSetBookmark(Item).FBookmark);
  FDataSet.Edit;

  if ColumnIndex > -1 then
    FDataSet.Fields[ColumnIndex].Value := Value
  else
    FDataSet.Fields[0].Value := Value;

  FDataSet.Post;
end;

{ TDataSetColumnDefinitions }

constructor TDataSetColumnDefinitions.Create(ADataSet: TDataSet);
var
  i: Integer;
begin
  inherited Create;

  FMainColumnIndex := -1;

  for i := 0 to ADataSet.Fields.Count - 1 do
    Add(ADataSet.Fields[i].DisplayName, ADataSet.Fields[i].DisplayWidth * 10);
end;

end.
