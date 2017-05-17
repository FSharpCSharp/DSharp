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

unit DSharp.Core.XmlSerialization.XmlWriter;

interface

uses
  DSharp.Core.XmlSerialization,
  Rtti,
  XMLIntf;

type
  TXmlWriter = class(TInterfacedObject, IXmlWriter)
  private
    fCurrentNode: IXMLNode;
    fDocument: IXMLDocument;
    fFilename: string;
    function GetXml: string;

    procedure WriteEnumerable(const instance: TObject);
    procedure WriteEvent(const value: TValue);
    procedure WriteInterface(const instance: IInterface);
    procedure WriteObject(const instance: TObject);
  public
    constructor Create; overload;
    constructor Create(const filename: string); overload;
    destructor Destroy; override;

    procedure WriteAttribute(const name: string; const value: TValue);
    procedure WriteEndElement(const name: string);
    procedure WriteStartElement(const name: string);
    procedure WriteValue(const value: TValue);

    property Xml: string read GetXml;
  end;

implementation

uses
  DSharp.Core.Reflection,
  SysUtils,
  TypInfo,
  XMLDoc,
  XSBuiltIns;

function TimeToXMLTime(Value: TDateTime): string;
begin
  Result := Copy(DateTimeToXMLTime(Value), 12, 18);
end;

{ TXmlWriter }

constructor TXmlWriter.Create;
begin
  inherited Create;

  fDocument := TXMLDocument.Create(nil);
  fDocument.Options := fDocument.Options + [doNodeAutoIndent];
  fDocument.Active := True;
  fDocument.Encoding := 'utf-8';
  fDocument.Version := '1.0';
end;

constructor TXmlWriter.Create(const filename: string);
begin
  Create;

  fFilename := filename;
end;

destructor TXmlWriter.Destroy;
begin
  if fFilename <> '' then
    fDocument.SaveToFile(fFilename);

  inherited;
end;

function TXmlWriter.GetXml: string;
begin
  Result := FormatXMLData(fDocument.XML.Text);
end;

procedure TXmlWriter.WriteAttribute(const name: string; const value: TValue);
var
  currentNode: IXMLNode;
begin
  currentNode := fCurrentNode;
  try
    fCurrentNode := fCurrentNode.AttributeNodes.Nodes[name];
    WriteValue(value);
  finally
    fCurrentNode := currentNode;
  end;
end;

procedure TXmlWriter.WriteEndElement(const name: string);
var
  LCurrentNode: IXMLNode;
begin
  if SameText(fCurrentNode.NodeName, name) then
  begin
    LCurrentNode := fCurrentNode;
    fCurrentNode := LCurrentNode.ParentNode;
  end;
end;

procedure TXmlWriter.WriteEnumerable(const instance: TObject);
var
  LEnumerator: TValue;
  LMethod: TRttiMethod;
  LProperty: TRttiProperty;
  LValue: TValue;
  LType: TRttiType;
  LFreeEnumerator: Boolean;
  LTypeName: string;
begin
  if instance.HasMethod('Add') and instance.TryGetMethod('GetEnumerator', LMethod) then
  begin
    LEnumerator := LMethod.Invoke(instance, []);
    LFreeEnumerator := LEnumerator.IsObject;
    try
      LType := LEnumerator.RttiType;
      if LType is TRttiInterfaceType then
      begin
        LEnumerator := LEnumerator.ToObject;
        LType := LEnumerator.RttiType;
      end;
      if LType.TryGetMethod('MoveNext', LMethod)
        and LType.TryGetProperty('Current', LProperty) then
      begin
        while LMethod.Invoke(LEnumerator, []).AsBoolean do
        begin
          LValue := LProperty.GetValue(LEnumerator.AsPointer);
          LTypeName := LProperty.PropertyType.Name;

          WriteStartElement(LTypeName);
          WriteValue(LValue);
          WriteEndElement(LTypeName);
        end;
      end;
    finally
      if LFreeEnumerator then
        LEnumerator.AsObject.Free;
    end;
  end;
end;

procedure TXmlWriter.WriteEvent(const value: TValue);
type
  PMethod = ^TMethod;
var
  LEvent: PMethod;
  LMethod: TRttiMethod;
begin
  LEvent := value.GetReferenceToRawData;
  if TObject(LEvent.Data).TryGetMethod(LEvent.Code, LMethod) then
    fCurrentNode.NodeValue := LMethod.Name;
end;

procedure TXmlWriter.WriteInterface(const instance: IInterface);
begin
  WriteObject(instance as TObject);
end;

procedure TXmlWriter.WriteObject(const instance: TObject);
var
  prop: TRttiProperty;
  value: TValue;
  elemAttribute: XmlElementAttribute;
  attrAttribute: XmlAttributeAttribute;
  field: TRttiField;
begin
  if Assigned(instance) then
  begin
    for prop in instance.GetProperties do
    begin
      if prop.IsDefined<XmlIgnoreAttribute> then
        Continue;

      value := prop.GetValue(instance);

      // don't write default values
      if (prop is TRttiInstanceProperty) and prop.PropertyType.IsOrdinal
        and (TRttiInstanceProperty(prop).Default = value.AsOrdinal) then
        Continue;

      // don't write nil objects
      if prop.PropertyType.IsInstance and (value.AsObject = nil) then
        Continue;

      if prop.TryGetCustomAttribute<XmlElementAttribute>(elemAttribute) then
      begin
        WriteStartElement(elemAttribute.ElementName);
        WriteValue(value);
        WriteEndElement(elemAttribute.ElementName);
      end else
      if prop.TryGetCustomAttribute<XmlAttributeAttribute>(attrAttribute) then
        WriteAttribute(attrAttribute.AttributeName, value)
      else if prop.Visibility = mvPublished then
      begin
        WriteStartElement(prop.Name);
        WriteValue(value);
        WriteEndElement(prop.Name);
      end;
    end;

    for field in instance.GetFields do
    begin
      if field.IsDefined<XmlIgnoreAttribute> then
        Continue;

      value := field.GetValue(instance);

      // don't write nil objects
      if field.FieldType.IsInstance and (value.AsObject = nil) then
        Continue;

      if field.TryGetCustomAttribute<XmlElementAttribute>(elemAttribute) then
      begin
        WriteStartElement(elemAttribute.ElementName);
        WriteValue(value);
        WriteEndElement(elemAttribute.ElementName);
      end else
      if field.TryGetCustomAttribute<XmlAttributeAttribute>(attrAttribute) then
        WriteAttribute(attrAttribute.AttributeName, value);
    end;

    WriteEnumerable(instance);
  end;
end;

procedure TXmlWriter.WriteStartElement(const name: string);
begin
  if Assigned(fCurrentNode) then
    fCurrentNode := fCurrentNode.AddChild(name)
  else
    fCurrentNode := fDocument.AddChild(name);
end;

procedure TXmlWriter.WriteValue(const value: TValue);
begin
  case value.Kind of
    tkInteger, tkInt64:
      fCurrentNode.NodeValue := value.ToString;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      fCurrentNode.NodeValue := value.ToString;
    tkEnumeration:
      fCurrentNode.NodeValue := value.ToString;
    tkFloat:
      if value.IsDate then
        fCurrentNode.NodeValue := DateToStr(value.AsDate, XmlFormatSettings)
      else if value.IsDateTime then
        fCurrentNode.NodeValue := DateTimeToXMLTime(value.AsDateTime)
      else if value.IsTime then
        fCurrentNode.NodeValue := TimeToXMLTime(value.AsTime)
      else
        fCurrentNode.NodeValue := FloatToStr(value.AsExtended, XmlFormatSettings);
    tkSet:
      fCurrentNode.NodeValue := value.ToString;
    tkClass:
      WriteObject(value.AsObject);
    tkMethod:
      WriteEvent(value);
    tkInterface:
      WriteInterface(value.AsInterface);

//    tkPointer,
//    tkProcedure:
//      FCurrentNode.NodeValue := AValue.AsType<TMethod>.ToString;
    // not supported yet
    tkRecord:
    begin
//      FCurrentNode.NodeValue := AValue.ToString;
//      for LProperty in AValue.GetType.GetProperties do
//      begin
//      end;
    end;
  end;
end;

end.
