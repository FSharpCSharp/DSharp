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

unit DSharp.Core.DataTemplates;

interface

uses
  DSharp.Collections;

type
  IDataTemplate = interface
    // methods to display items
    function GetImageIndex(const Item: TObject; const ColumnIndex: Integer): Integer;
    function GetText(const Item: TObject; const ColumnIndex: Integer): string;

    // methods to edit items
    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string);

    // methods to build the tree structure
    function GetItem(const Item: TObject; const Index: Integer): TObject;
    function GetItemCount(const Item: TObject): Integer;
    function GetItems(const Item: TObject): IList<TObject>;
    function GetItemTemplate(const Item: TObject): IDataTemplate;

    // methods to manage the template "binding"
    function GetTemplateDataClass: TClass;
    procedure RegisterDataTemplate(const DataTemplate: IDataTemplate);
  end;

  TDataTemplate = class(TInterfacedObject, IDataTemplate)
  private
    FTemplates: IList<IDataTemplate>;
  protected
    property Templates: IList<IDataTemplate> read FTemplates;
  public
    constructor Create;
    destructor Destroy; override;

    function GetImageIndex(const Item: TObject;
      const ColumnIndex: Integer): Integer; virtual;
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; virtual;

    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string); virtual;

    function GetItem(const Item: TObject;
      const Index: Integer): TObject; virtual;
    function GetItemCount(const Item: TObject): Integer; virtual;
    function GetItems(const Item: TObject): IList<TObject>; virtual;
    function GetItemTemplate(const Item: TObject): IDataTemplate; virtual;

    function GetTemplateDataClass: TClass; virtual;
    procedure RegisterDataTemplate(const DataTemplate: IDataTemplate);
  end;

  TDataTemplate<T: class> = class(TDataTemplate)
  public
    function GetImageIndex(const Item: TObject;
      const ColumnIndex: Integer): Integer; overload; override; final;
    function GetImageIndex(const Item: T;
      const ColumnIndex: Integer): Integer; reintroduce; overload; virtual;

    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string); overload; override; final;
    procedure SetText(const Item: T; const ColumnIndex: Integer;
      const Value: string); reintroduce; overload; virtual;

    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; overload; override; final;
    function GetText(const Item: T;
      const ColumnIndex: Integer): string; reintroduce; overload; virtual;
    function GetItem(const Item: TObject;
      const Index: Integer): TObject; overload; override; final;
    function GetItem(const Item: T;
      const Index: Integer): TObject; reintroduce; overload; virtual;
    function GetItemCount(const Item: TObject): Integer; overload; override; final;
    function GetItemCount(const Item: T): Integer; reintroduce; overload; virtual;

    function GetTemplateDataClass: TClass; override;
  end;

implementation

uses
  DSharp.Core.Reflection;

{ TDataTemplate }

constructor TDataTemplate.Create;
begin
  FTemplates := TList<IDataTemplate>.Create();
end;

destructor TDataTemplate.Destroy;
begin
  inherited;
end;

function TDataTemplate.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
begin
  Result := -1;
end;

function TDataTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  Result := nil;

  if IsClassCovariantTo(Item.ClassType, TList<TObject>)
    and (TList<TObject>(Item).Count > Index) then
  begin
    Result := TList<TObject>(Item).Items[Index];
  end;
end;

function TDataTemplate.GetItemCount(const Item: TObject): Integer;
begin
  Result := 0;

  if Assigned(Item) and IsClassCovariantTo(Item.ClassType, TList<TObject>) then
  begin
    Result := TList<TObject>(Item).Count;
  end;
end;

function TDataTemplate.GetItems(const Item: TObject): IList<TObject>;
begin
  Result := nil;
end;

function TDataTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
var
  LTemplate: IDataTemplate;
begin
  Result := nil;

  for LTemplate in FTemplates do
  begin
    Result := LTemplate.GetItemTemplate(Item);
    if Assigned(Result) then
    begin
      Break;
    end;
  end;

  if not Assigned(Result) and Assigned(Item) and (Item.InheritsFrom(GetTemplateDataClass)
    or IsClassCovariantTo(Item.ClassType, GetTemplateDataClass)) then
  begin
    Result := Self;
  end;
end;

function TDataTemplate.GetTemplateDataClass: TClass;
begin
  Result := TList<TObject>;
end;

function TDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := '(not available)';
end;

procedure TDataTemplate.RegisterDataTemplate(const DataTemplate: IDataTemplate);
begin
  FTemplates.Add(DataTemplate);
end;

procedure TDataTemplate.SetText(const Item: TObject; const ColumnIndex: Integer;
  const Value: string);
begin

end;

{ TDataTemplate<T> }

function TDataTemplate<T>.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
begin
  Result := GetImageIndex(T(Item), ColumnIndex);
end;

function TDataTemplate<T>.GetImageIndex(const Item: T;
  const ColumnIndex: Integer): Integer;
begin
  Result := inherited GetImageIndex(Item, ColumnIndex);
end;

function TDataTemplate<T>.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  Result := GetItem(T(Item), Index);
end;

function TDataTemplate<T>.GetItem(const Item: T; const Index: Integer): TObject;
begin
  Result := inherited GetItem(Item, Index);
end;

function TDataTemplate<T>.GetItemCount(const Item: TObject): Integer;
begin
  Result := GetItemCount(T(Item));
end;

function TDataTemplate<T>.GetItemCount(const Item: T): Integer;
begin
  Result := inherited GetItemCount(Item);
end;

function TDataTemplate<T>.GetTemplateDataClass: TClass;
begin
  Result := T;
end;

function TDataTemplate<T>.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := GetText(T(Item), ColumnIndex);
end;

function TDataTemplate<T>.GetText(const Item: T;
  const ColumnIndex: Integer): string;
begin
  Result := inherited GetText(Item, ColumnIndex);
end;

procedure TDataTemplate<T>.SetText(const Item: TObject;
  const ColumnIndex: Integer; const Value: string);
begin
  SetText(T(Item), ColumnIndex, Value);
end;

procedure TDataTemplate<T>.SetText(const Item: T; const ColumnIndex: Integer;
  const Value: string);
begin
  inherited SetText(Item, ColumnIndex, Value);
end;

end.
