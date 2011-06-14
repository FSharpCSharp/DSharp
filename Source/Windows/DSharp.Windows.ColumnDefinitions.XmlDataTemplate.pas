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

unit DSharp.Windows.ColumnDefinitions.XmlDataTemplate;

interface

uses
  DSharp.Core.XNode,
  DSharp.Windows.ColumnDefinitions.DataTemplate;

type
  TXmlDataTemplate = class(TColumnDefinitionsDataTemplate)
  public
    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;

    function GetItem(const Item: TObject; const Index: Integer): TObject; override;
    function GetItemCount(const Item: TObject): Integer; override;

    function GetTemplateDataClass: TClass; override;
  end;

implementation

{ TXmlDataTemplate }

function TXmlDataTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  if Assigned(Item) then
  begin
    Result := TXNode(Item).ChildNodes[Index];
  end
  else
  begin
    Result := nil;
  end;
end;

function TXmlDataTemplate.GetItemCount(const Item: TObject): Integer;
begin
  if Assigned(Item) then
  begin
    Result := TXNode(Item).ChildNodes.Count;
  end
  else
  begin
    Result := 0;
  end;
end;

function TXmlDataTemplate.GetTemplateDataClass: TClass;
begin
  Result := TXNode;
end;

function TXmlDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    with FColumnDefinitions[ColumnIndex].Binding do
    begin
      if Assigned(FColumnDefinitions[ColumnIndex].OnGetText) then
      begin
        Result := FColumnDefinitions[ColumnIndex].OnGetText(
          FColumnDefinitions.Owner, FColumnDefinitions[ColumnIndex], Item);
      end
      else
      begin
        Result := TXNode(Item).SelectValue(SourcePropertyName);
      end;
    end;
  end;
end;

end.
