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

unit Collections.Xml;

interface

uses
  Generics.Collections,
  System.PropertyChangedBase,
  XMLDoc,
  xmldom,
  XMLIntf,
  msxmldom;

type
  TXNode = class(TPropertyChangedBase)
  private
    FChildNodes: TList<TXNode>;
    FDOMNode: IDOMNode;
    function GetChildNodes: TList<TXNode>;
    function GetName: string;
    function GetTextNode(ANode: IDOMNode): IDOMNode;
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(ANode: IDOMNode);
    destructor Destroy; override;

    function SelectElement(const XPath: string): IDOMNode; overload;
    function SelectElements(const XPath: string): IDOMNodeList; overload;
    class function SelectElement(Node: IDOMNode; const XPath: string): IDOMNode; overload;
    class function SelectElements(Node: IDOMNode; const XPath: string): IDOMNodeList; overload;
    function SelectValue(const XPath: string = ''): string;

    property ChildNodes: TList<TXNode> read GetChildNodes;
    property DOMNode: IDOMNode read FDOMNode;
    property Name: string read GetName;
    property Value: string read GetValue write SetValue;
  end;

  TXmlDocumentHelper = class helper for TXmlDocument
  public
    function SelectElement(const XPath: string): TXNode; overload;
    function SelectElement(Node: IXmlNode; const XPath: string): TXNode; overload;
    function SelectElements(const XPath: string): TArray<TXNode>; overload;
    function SelectElements(Node: IXMLNode; const XPath: string): TArray<TXNode>; overload;
  end;

implementation

uses
  Collections.ObservableCollection,
  SysUtils;

{ TXNode }

constructor TXNode.Create(ANode: IDOMNode);
begin
  FChildNodes := TObjectList<TXNode>.Create();
  FDOMNode := ANode;
end;

destructor TXNode.Destroy;
begin
  FChildNodes.Free();
  inherited;
end;

function TXNode.GetChildNodes: TList<TXNode>;
var
  i: Integer;
begin
  // do it the easy way for now
  if FChildNodes.Count = 0then
  begin
    for i := 0 to Pred(FDOMNode.childNodes.length) do
    begin
      if FDOMNode.childNodes[i].nodeType = ELEMENT_NODE then
      begin
        FChildNodes.Add(TXNode.Create(FDOMNode.childNodes[i]));
      end;
    end;
  end;

  Result := FChildNodes;
end;

function TXNode.GetName: string;
begin
  Result := FDOMNode.nodeName;
end;

function TXNode.GetTextNode(ANode: IDOMNode): IDOMNode;
begin
  if Assigned(ANode) then
  begin
    if ANode.nodeType = ELEMENT_NODE then
    begin
      if ANode.hasChildNodes then
      begin
        Result := ANode.childNodes[0];
      end
      else
      begin
        Result := ANode.appendChild(ANode.ownerDocument.createTextNode(''));
      end;
    end
    else
    begin
      Result := ANode;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

function TXNode.GetValue: string;
begin
  Result := Trim(GetTextNode(FDOMNode).nodeValue);
end;

function TXNode.SelectElement(const XPath: string): IDOMNode;
begin
  Result := SelectElement(FDOMNode, XPath);
end;

class function TXNode.SelectElement(Node: IDOMNode; const XPath: string): IDOMNode;
var
  LNodeSelect: IDOMNodeSelect;
begin
  Result := nil;
  if Supports(Node, IDomNodeSelect, LNodeSelect) then
  begin
    Result := LNodeSelect.selectNode(XPath);
  end;
end;

function TXNode.SelectElements(const XPath: string): IDOMNodeList;
begin
  Result := SelectElements(FDOMNode, XPath);
end;

class function TXNode.SelectElements(Node: IDOMNode; const XPath: string): IDOMNodeList;
var
  LNodeSelect: IDOMNodeSelect;
begin
  Result := nil;
  if Supports(Node, IDomNodeSelect, LNodeSelect) then
  begin
    Result := LNodeSelect.selectNodes(XPath);
  end;
end;

function TXNode.SelectValue(const XPath: string): string;
var
  LNode: IDOMNode;
begin
  Result := '';

  if XPath = '' then
    LNode := FDOMNode
  else
    LNode := SelectElement(XPath);

  LNode := GetTextNode(LNode);

  if Assigned(LNode) then
    Result := Trim(LNode.nodeValue);
end;

procedure TXNode.SetValue(const Value: string);
begin
  GetTextNode(FDOMNode).nodeValue := Value;
  DoPropertyChanged('Value');
end;

{ TXmlDocumentHelper }

function TXmlDocumentHelper.SelectElement(const XPath: string): TXNode;
begin
  Result := SelectElement(Node, XPath);
end;

function TXmlDocumentHelper.SelectElement(Node: IXmlNode;
  const XPath: string): TXNode;
var
  LNode: IDOMNode;
begin
  Result := nil;
  LNode := TXNode.SelectElement(Node.DOMNode, XPath);
  if Assigned(LNode) then
  begin
    Result := TXNode.Create(LNode);
  end;
end;

function TXmlDocumentHelper.SelectElements(const XPath: string): TArray<TXNode>;
begin
  Result := SelectElements(Node, XPath);
end;

function TXmlDocumentHelper.SelectElements(Node: IXMLNode;
  const XPath: string): TArray<TXNode>;
var
  LNodes: IDOMNodeList;
  i: Integer;
begin
  Result := nil;
  LNodes := TXNode.SelectElements(Node.DOMNode, XPath);
  if Assigned(LNodes) then
  begin
    SetLength(Result, LNodes.length);
    for i := 0 to Pred(LNodes.length) do
    begin
      Result[i] := TXNode.Create(LNodes[i]);
    end;
  end;
end;

end.
