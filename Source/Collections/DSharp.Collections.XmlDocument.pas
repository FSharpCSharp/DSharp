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

unit DSharp.Collections.XmlDocument;

interface

uses
  DSharp.Core.XNode,
  Spring.Collections,
  XMLDoc,
  xmldom,
  XMLIntf,
  msxmldom;

type
  TXmlDocumentHelper = class helper for TXmlDocument
  public
    function SelectElement(const XPath: string): TXNode; overload;
    function SelectElement(Node: IXmlNode; const XPath: string): TXNode; overload;
    function SelectElements(const XPath: string): TArray<TXNode>; overload;
    function SelectElements(Node: IXMLNode; const XPath: string): TArray<TXNode>; overload;
  end;

implementation

uses
  DSharp.Collections.ObservableCollection;

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
