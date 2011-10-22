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

unit DSharp.DevExpress.PresenterDataSource;

interface

uses
  cxTL,
  cxTLData,
  cxCustomData,
  DSharp.Windows.CustomPresenter;

type
  TPresenterDataSource = class(TcxTreeListCustomDataSource)
  private
    FPresenter: TCustomPresenter;
  protected
    // Smart load mode (used in tree)
    function GetChildCount(AParentHandle: TcxDataRecordHandle): Integer; override;
    function GetChildRecordHandle(AParentHandle: TcxDataRecordHandle;
      AChildIndex: Integer): TcxDataRecordHandle; override;
    function GetRootRecordHandle: TcxDataRecordHandle; override;

    // Load all records mode
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;

    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(APresenter: TCustomPresenter);
  end;

implementation

uses
  DSharp.Core.DataTemplates,
  Variants;

{ TPresenterDataSource }

constructor TPresenterDataSource.Create(APresenter: TCustomPresenter);
begin
  FPresenter := APresenter;
end;

function TPresenterDataSource.GetChildCount(
  AParentHandle: TcxDataRecordHandle): Integer;
var
  LItemTemplate: IDataTemplate;
begin
  Result := 0;

  LItemTemplate := FPresenter.GetItemTemplate(AParentHandle);
  if Assigned(LItemTemplate) then
  begin
    Result := LItemTemplate.GetItemCount(AParentHandle);
  end;
end;

function TPresenterDataSource.GetChildRecordHandle(AParentHandle: TcxDataRecordHandle;
  AChildIndex: Integer): TcxDataRecordHandle;
var
  LItemTemplate: IDataTemplate;
begin
  Result := nil;

  LItemTemplate := FPresenter.GetItemTemplate(AParentHandle);
  if Assigned(LItemTemplate) then
  begin
    Result := LItemTemplate.GetItem(AParentHandle, AChildIndex);
  end;
end;

function TPresenterDataSource.GetRecordCount: Integer;
begin
  Result := GetChildCount(GetRootRecordHandle);
end;

function TPresenterDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := GetChildRecordHandle(GetRootRecordHandle, ARecordIndex);
end;

function TPresenterDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(FPresenter.View.ItemsSource as TObject);
end;

function TPresenterDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  LItemTemplate: IDataTemplate;
begin
  LItemTemplate := FPresenter.GetItemTemplate(ARecordHandle);
  Result := LItemTemplate.GetText(ARecordHandle, Integer(AItemHandle));
end;

procedure TPresenterDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  LItemTemplate: IDataTemplate;
begin
  LItemTemplate := FPresenter.GetItemTemplate(ARecordHandle);
  LItemTemplate.SetText(ARecordHandle, Integer(AItemHandle), VarToStrDef(AValue, ''));
end;

end.
