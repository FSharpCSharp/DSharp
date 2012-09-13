(*
  Copyright (c) 2011-2012, Stefan Glienke
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
    FCurrentNode: IXMLNode;
    FDocument: IXMLDocument;
    function GetXml: string;

    procedure WriteEnumerable(const AValue: TValue);
    procedure WriteEvent(const AValue: TValue);
    procedure WriteObject(const AValue: TValue);
  public
    constructor Create;

    procedure WriteStartElement(const AName: string);
    procedure WriteEndElement(const AName: string);
    procedure WriteValue(const AValue: TValue);

    property Xml: string read GetXml;
  end;

implementation

uses
  DSharp.Core.Reflection,
  SysUtils,
  TypInfo,
  XMLDoc,
  XSBuiltIns;

type
  PMethod = ^TMethod;

function TimeToXMLTime(Value: TDateTime): string;
begin
  Result := Copy(DateTimeToXMLTime(Value), 12, 18);
end;

{ TXmlWriter }

constructor TXmlWriter.Create;
begin
  FDocument := TXMLDocument.Create(nil);
  FDocument.Options := FDocument.Options + [doNodeAutoIndent];
  FDocument.Active := True;
  FDocument.Encoding := 'utf-8';
  FDocument.Version := '1.0';
end;

function TXmlWriter.GetXml: string;
begin
  Result := FormatXMLData(FDocument.XML.Text);
end;

procedure TXmlWriter.WriteEndElement(const AName: string);
var
  LCurrentNode: IXMLNode;
begin
  if SameText(FCurrentNode.NodeName, AName) then
  begin
    LCurrentNode := FCurrentNode;
    FCurrentNode := LCurrentNode.ParentNode;
    if not LCurrentNode.HasChildNodes then
    begin
      FCurrentNode.ChildNodes.Remove(LCurrentNode);
    end;
  end;
end;

procedure TXmlWriter.WriteEnumerable(const AValue: TValue);
var
  LObject: TObject;
//  LEnumerator: TObject;
  LEnumerator: TValue;
  LMethod: TRttiMethod;
  LProperty: TRttiProperty;
  LValue: TValue;
  LType: TRttiType;
begin
  LObject := AValue.AsObject;
  if LObject.HasMethod('Add') and LObject.TryGetMethod('GetEnumerator', LMethod) then
  begin
    LEnumerator := LMethod.Invoke(LObject, []);

    try
      LType := LEnumerator.RttiType;
      if LType.TryGetMethod('MoveNext', LMethod)
        and LType.TryGetProperty('Current', LProperty) then
      begin
        while LMethod.Invoke(LEnumerator, []).AsBoolean do
        begin
          if LEnumerator.IsObject then
          begin
            LValue := LProperty.GetValue(LEnumerator.AsObject);
          end else
          if LEnumerator.IsInterface then
          begin
            LValue := LProperty.GetValue(Pointer(LEnumerator.AsInterface));
          end else
          begin
            LValue := LProperty.GetValue(LEnumerator.GetReferenceToRawData);
          end;

          WriteStartElement(LProperty.PropertyType.Name);
          WriteValue(LValue);
          WriteEndElement(LProperty.PropertyType.Name);
        end;
      end;
    finally
      if LEnumerator.IsObject then
        LEnumerator.AsObject.Free();
    end;
  end;
end;

procedure TXmlWriter.WriteEvent(const AValue: TValue);
var
  LEvent: PMethod;
  LMethod: TRttiMethod;
begin
  LEvent := AValue.GetReferenceToRawData();
  if TObject(LEvent.Data).TryGetMethod(LEvent.Code, LMethod) then
  begin
    FCurrentNode.NodeValue := LMethod.Name;
  end;
end;

procedure TXmlWriter.WriteObject(const AValue: TValue);
var
  LObject: TObject;
  LProperty: TRttiProperty;
  LValue: TValue;
  LAttribute: XmlElementAttribute;
  LField: TRttiField;
begin
  LObject := AValue.AsObject;
  if Assigned(LObject) then
  begin
    for LProperty in LObject.GetProperties() do
    begin
      if (LProperty.Visibility = mvPublished)
        and LProperty.TryGetValue(LObject, LValue)
        and not LProperty.HasAttributeOfType<XmlIgnoreAttribute> then
      begin
        if not (LProperty is TRttiInstanceProperty)
          or not LProperty.PropertyType.IsOrdinal
          or (LProperty.PropertyType.IsOrdinal
          and (TRttiInstanceProperty(LProperty).Default <> LValue.AsOrdinal))
          or (LProperty.PropertyType.IsInstance and (LValue.AsObject <> nil)) then
        begin
          if LProperty.TryGetAttributeOfType<XmlElementAttribute>(LAttribute) then
          begin
            WriteStartElement(LAttribute.ElementName);
            WriteValue(LValue);
            WriteEndElement(LAttribute.ElementName);
          end
          else
          begin
            WriteStartElement(LProperty.Name);
            WriteValue(LValue);
            WriteEndElement(LProperty.Name);
          end;
        end;
      end;
    end;

    for LField in LObject.GetType.GetFields do
    begin
      if (LField.Visibility = mvPublished)
        and LField.TryGetValue(LObject, LValue)
        and not LField.HasAttributeOfType<XmlIgnoreAttribute> then
      begin
        if not LField.FieldType.IsInstance or (LValue.AsObject <> nil) then
        begin
          if LField.TryGetAttributeOfType<XmlElementAttribute>(LAttribute) then
          begin
            WriteStartElement(LAttribute.ElementName);
            WriteValue(LValue);
            WriteEndElement(LAttribute.ElementName);
          end
          else
          begin
            WriteStartElement(LField.Name);
            WriteValue(LValue);
            WriteEndElement(LField.Name);
          end;
        end;
      end;
    end;

    WriteEnumerable(AValue);
  end;
end;

procedure TXmlWriter.WriteStartElement(const AName: string);
begin
  if Assigned(FCurrentNode) then
  begin
    FCurrentNode := FCurrentNode.AddChild(AName);
  end
  else
  begin
    FCurrentNode := FDocument.AddChild(AName);
  end;
end;

procedure TXmlWriter.WriteValue(const AValue: TValue);
begin
  case AValue.Kind of
    tkInteger, tkInt64:
    begin
      FCurrentNode.NodeValue := AValue.ToString;
    end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
    begin
      FCurrentNode.NodeValue := AValue.ToString;
    end;
    tkEnumeration:
    begin
      FCurrentNode.NodeValue := AValue.ToString;
    end;
    tkFloat:
    begin
      if AValue.IsDate then
      begin
        FCurrentNode.NodeValue := DateToStr(AValue.AsDate, XmlFormatSettings);
      end else
      if AValue.IsDateTime then
      begin
        FCurrentNode.NodeValue := DateTimeToXMLTime(AValue.AsDateTime);
      end else
      if AValue.IsTime then
      begin
        FCurrentNode.NodeValue := TimeToXMLTime(AValue.AsTime);
      end
      else
      begin
        FCurrentNode.NodeValue := FloatToStr(AValue.AsExtended, XmlFormatSettings);
      end;
    end;
    tkSet:
    begin
      FCurrentNode.NodeValue := AValue.ToString;
    end;
    tkClass:
    begin
      WriteObject(AValue);
    end;
    tkMethod:
    begin
      WriteEvent(AValue);
    end;

//    tkPointer,
//    tkProcedure:
//      FCurrentNode.NodeValue := AValue.AsType<TMethod>.ToString;
    // not supported yet
    tkRecord:
    begin
//      FCurrentNode.NodeValue := AValue.ToString;
//      for LProperty in AValue.GetType().GetProperties() do
//      begin
//      end;
    end;
  end;
end;

end.
