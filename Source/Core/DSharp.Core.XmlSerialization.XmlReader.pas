(*
  Copyright (c) 2011-2014, Stefan Glienke
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

unit DSharp.Core.XmlSerialization.XmlReader;

interface

uses
  DSharp.Core.XmlSerialization,
  Rtti,
  XMLIntf;

type
  TXmlReader = class(TInterfacedObject, IXmlReader)
  private
    fAttributeIndex: Integer;
    fCurrentNode: IXMLNode;
    fDocument: IXMLDocument;
    fElementNode: IXMLNode;
    fIndex: Integer;
    fRoot: TObject;
    function GetXml: string;
    procedure SetXml(const Value: string);

    procedure CreateObject(var value: TValue);

    procedure ReadAttribute(const instance: TObject);
    procedure ReadElement(const instance: TObject);
    procedure ReadEnumerable(const instance: TObject);
    procedure ReadEvent(var value: TValue);
    procedure ReadInterface(const instance: IInterface);
    procedure ReadProperty(const instance: TObject; const prop: TRttiProperty);
    procedure ReadObject(const instance: TObject);
  public
    constructor Create; overload;
    constructor Create(const filename: string); overload;

    function IsStartElement: Boolean; overload;
    function IsStartElement(const name: string): Boolean; overload;

    function MoveToElement: Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;

    procedure ReadStartElement; overload;
    procedure ReadStartElement(const name: string); overload;
    procedure ReadEndElement; overload;
    procedure ReadValue(var value: TValue);

    property Xml: string read GetXml write SetXml;
  end;

implementation

uses
  DSharp.Core.Reflection,
  SysUtils,
  TypInfo,
  Variants,
  XMLDoc,
  XSBuiltIns;

function XMLTimeToTime(const XMLTime: string): TDateTime;
begin
  Result := Frac(XMLTimeToDateTime(FormatDateTime('yyyy-mm-dd''T', 0) + XMLTime));
end;

{ TXmlReader }

constructor TXmlReader.Create;
begin
  inherited Create;

  fDocument := TXMLDocument.Create(nil);
  fDocument.Active := True;
end;

constructor TXmlReader.Create(const filename: string);
begin
  Create;

  if filename <> '' then
    fDocument.LoadFromFile(filename);
end;

procedure TXmlReader.CreateObject(var value: TValue);
var
  rttiType: TRttiType;
  method: TRttiMethod;
begin
  if FindType(fCurrentNode.NodeName, rttiType)
    and rttiType.TryGetStandardConstructor(method) then
  begin
    value := method.Invoke(rttiType.AsInstance.MetaclassType, []);
    if not Assigned(fRoot) then
      fRoot := value.AsObject;
  end;
end;

function TXmlReader.GetXml: string;
begin
  Result := FormatXMLData(fDocument.XML.Text);
end;

function TXmlReader.IsStartElement: Boolean;
begin
  if Assigned(fCurrentNode) then
    Result := fCurrentNode.ChildNodes.Count > fIndex
  else
    Result := Assigned(fDocument.DocumentElement);
end;

function TXmlReader.IsStartElement(const name: string): Boolean;
begin
  if Assigned(fCurrentNode) then
    Result := (fCurrentNode.ChildNodes.Count > fIndex)
      and SameText(fCurrentNode.ChildNodes[fIndex].NodeName, name)
  else
    Result := SameText(fDocument.DocumentElement.NodeName, name);
end;

function TXmlReader.MoveToElement: Boolean;
begin
  Result := Assigned(fElementNode);
  if Result then
  begin
    fAttributeIndex := -1;
    fCurrentNode := fElementNode;
    fElementNode := nil;
  end;
end;

function TXmlReader.MoveToFirstAttribute: Boolean;
begin
  Result := fCurrentNode.AttributeNodes.Count > 0;
  if Result then
  begin
    fAttributeIndex := 0;
    fElementNode := fCurrentNode;
    fCurrentNode := fCurrentNode.AttributeNodes.First;
  end;
end;

function TXmlReader.MoveToNextAttribute: Boolean;
begin
  if not Assigned(fElementNode) then
    Result := MoveToFirstAttribute
  else
  begin
    Result := fAttributeIndex < fElementNode.AttributeNodes.Count - 1;
    if Result then
    begin
      Inc(fAttributeIndex);
      fCurrentNode := fElementNode.AttributeNodes.Nodes[fAttributeIndex];
    end;
  end;
end;

procedure TXmlReader.ReadAttribute(const instance: TObject);
var
  prop: TRttiProperty;
  attrAttribute: XmlAttributeAttribute;
begin
  for prop in instance.GetProperties do
  begin
    if prop.TryGetCustomAttribute<XmlAttributeAttribute>(attrAttribute)
      and SameText(attrAttribute.AttributeName, fCurrentNode.NodeName) then
    begin
      ReadProperty(instance, prop);
      Break;
    end;
  end;
end;

procedure TXmlReader.ReadElement(const instance: TObject);
var
  prop: TRttiProperty;
  elemAttribute: XmlElementAttribute;
begin
  for prop in instance.GetProperties do
  begin
    if prop.TryGetCustomAttribute<XmlElementAttribute>(elemAttribute) then
    begin
      if SameText(elemAttribute.ElementName, fCurrentNode.NodeName) then
      begin
        ReadProperty(instance, prop);
        Break;
      end;
      Continue;
    end;

    if prop.IsDefined<XmlAttributeAttribute> then
      Continue;

    if prop.Visibility < mvPublished then
      Continue;

    if SameText(prop.Name, fCurrentNode.NodeName) then
    begin
      ReadProperty(instance, prop);
      Break;
    end;
  end;
end;

procedure TXmlReader.ReadEndElement;
begin
  fIndex := fCurrentNode.ParentNode.ChildNodes.IndexOf(fCurrentNode) + 1;
  fCurrentNode := fCurrentNode.ParentNode;
end;

procedure TXmlReader.ReadEnumerable(const instance: TObject);
var
  rttiType: TRttiType;
  method: TRttiMethod;
  value: TValue;
begin
  if instance.TryGetMethod('Clear', method) then
    method.Invoke(instance, []);

  if instance.HasMethod('GetEnumerator')
    and instance.TryGetMethod('Add', method) then
  begin
    rttiType := method.GetParameters[0].ParamType;
    while IsStartElement(rttiType.Name)
      or (rttiType.IsPublicType and IsStartElement(rttiType.QualifiedName)) do
    begin
      ReadStartElement;

      TValue.Make(nil, rttiType.Handle, value);
      ReadValue(value);
      method.Invoke(instance, [value]);

      ReadEndElement;
    end;
  end;
end;

procedure TXmlReader.ReadEvent(var value: TValue);
type
  PMethod = ^TMethod;
var
  event: PMethod;
  method: TRttiMethod;
begin
  event := value.GetReferenceToRawData;
  if fRoot.TryGetMethod(VarToStrDef(fCurrentNode.NodeValue, ''), method) then
  begin
    event.Data := fRoot;
    event.Code := method.CodeAddress;
  end;
end;

procedure TXmlReader.ReadInterface(const instance: IInterface);
begin
  ReadObject(instance as TObject);
end;

procedure TXmlReader.ReadObject(const instance: TObject);
begin
  ReadEnumerable(instance);

  if MoveToFirstAttribute then
  repeat
    ReadAttribute(instance);
  until not MoveToNextAttribute;
  MoveToElement;

  while IsStartElement do
  begin
    ReadStartElement;
    ReadElement(instance);
    ReadEndElement;
  end;
end;

procedure TXmlReader.ReadProperty(const instance: TObject;
  const prop: TRttiProperty);
var
  value: TValue;
begin
  if prop.IsReadable then
    value := prop.GetValue(instance);
  ReadValue(value);
  if prop.IsWritable and not (prop.PropertyType.IsInstance or prop.PropertyType.IsInterface) then
    prop.SetValue(instance, value);
end;

procedure TXmlReader.ReadStartElement;
begin
  if Assigned(fCurrentNode) then
    fCurrentNode := fCurrentNode.ChildNodes[fIndex]
  else
    fCurrentNode := fDocument.DocumentElement;
  fIndex := 0;
end;

procedure TXmlReader.ReadStartElement(const name: string);
begin
  if IsStartElement(name) then
    ReadStartElement
  else
    raise EXmlException.CreateFmt('Element "%s" not found', [name]);
end;

procedure TXmlReader.ReadValue(var value: TValue);
begin
  if value.IsEmpty then
    CreateObject(value);

  case value.Kind of
    tkInteger, tkInt64:
      value := TValue.FromOrdinal(value.TypeInfo, StrToInt64(fCurrentNode.NodeValue));
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      value := TValue.From<string>(VarToStrDef(fCurrentNode.NodeValue, ''));
    tkEnumeration:
      value := TValue.FromOrdinal(value.TypeInfo,
        GetEnumValue(value.TypeInfo, fCurrentNode.NodeValue));
    tkFloat:
    begin
      if value.IsDate then
        value := TValue.From<TDate>(StrToDate(fCurrentNode.NodeValue, XmlFormatSettings))
      else if value.IsDateTime then
        value := TValue.From<TDateTime>(XMLTimeToDateTime(fCurrentNode.NodeValue))
      else if value.IsTime then
        value := TValue.From<TTime>(XMLTimeToTime(fCurrentNode.NodeValue))
      else
        value := StrToFloat(fCurrentNode.NodeValue, XmlFormatSettings);
    end;
    tkSet:
      TValue.Make(StringToSet(value.TypeInfo, fCurrentNode.NodeValue),
        value.TypeInfo, value);
    tkClass:
      ReadObject(value.AsObject);
    tkMethod:
      ReadEvent(value);
    tkInterface:
      ReadInterface(value.AsInterface);
  end;
end;

procedure TXmlReader.SetXml(const Value: string);
begin
  fDocument.XML.Text := Value;
  fDocument.Active := True;
  fCurrentNode := nil;
  fIndex := 0;
end;

end.
