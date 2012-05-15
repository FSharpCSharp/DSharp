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

unit DSharp.Core.XmlSerialization.XmlReader;

interface

uses
  DSharp.Core.XmlSerialization,
  Rtti,
  SysUtils,
  XMLIntf;

type
  TXmlReader = class(TInterfacedObject, IXmlReader)
  private
    FCurrentNode: IXMLNode;
    FDocument: IXMLDocument;
    FIndex: Integer;
    FRoot: TObject;
    function GetXml: string;
    procedure SetXml(const Value: string);

    procedure CreateObject(var AValue: TValue);

    procedure ReadEnumerable(var AValue: TValue);
    procedure ReadEvent(var AValue: TValue);
    procedure ReadObject(var AValue: TValue);
  public
    constructor Create;
    destructor Destroy; override;

    function IsStartElement(): Boolean; overload;
    function IsStartElement(const AName: string): Boolean; overload;
    procedure ReadStartElement; overload;
    procedure ReadStartElement(const AName: string); overload;
    procedure ReadEndElement; overload;
    procedure ReadValue(var AValue: TValue);

    property Xml: string read GetXml write SetXml;
  end;

implementation

uses
  DSharp.Core.Reflection,
  TypInfo,
  Variants,
  XMLDoc,
  XSBuiltIns;

type
  PMethod = ^TMethod;

function XMLTimeToTime(const XMLTime: string): TDateTime;
begin
  Result := Frac(XMLTimeToDateTime(FormatDateTime('yyyy-mm-dd''T', 0) + XMLTime));
end;

{ TXmlReader }

constructor TXmlReader.Create;
begin
  FDocument := TXMLDocument.Create(nil);
  FDocument.Active := True;
end;

destructor TXmlReader.Destroy;
begin

  inherited;
end;

procedure TXmlReader.CreateObject(var AValue: TValue);
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LArgs: TArray<TValue>;
begin
  if FindType(FCurrentNode.NodeName, LType)
    and LType.TryGetStandardConstructor(LMethod) then
  begin
    SetLength(LArgs, LMethod.ParameterCount);
    AValue := LMethod.Invoke(LType.AsInstance.MetaclassType, LArgs);
    if not Assigned(FRoot) then
    begin
      FRoot := AValue.AsObject();
    end;
  end;
end;

function TXmlReader.GetXml: string;
begin
  Result := FormatXMLData(FDocument.XML.Text);
end;

function TXmlReader.IsStartElement: Boolean;
begin
  if Assigned(FCurrentNode) then
  begin
    Result := FCurrentNode.ChildNodes.Count > FIndex;
  end
  else
  begin
    Result := Assigned(FDocument.DocumentElement);
  end;
end;

function TXmlReader.IsStartElement(const AName: string): Boolean;
begin
  if Assigned(FCurrentNode) then
  begin
    Result := (FCurrentNode.ChildNodes.Count > FIndex)
      and SameText(FCurrentNode.ChildNodes[FIndex].NodeName, AName);
  end
  else
  begin
    Result := SameText(FDocument.DocumentElement.NodeName, AName);
  end;
end;

procedure TXmlReader.ReadEndElement;
begin
  FIndex := FCurrentNode.ParentNode.ChildNodes.IndexOf(FCurrentNode) + 1;
  FCurrentNode := FCurrentNode.ParentNode;
end;

procedure TXmlReader.ReadEnumerable(var AValue: TValue);
var
  LObject: TObject;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LValue: TValue;
begin
  LObject := AValue.AsObject;

  if LObject.HasMethod('GetEnumerator')
    and LObject.TryGetMethod('Add', LMethod) then
  begin
    LType := LMethod.GetParameters[0].ParamType;
    while IsStartElement(LType.Name) do
    begin
      ReadStartElement();

      TValue.Make(nil, LType.Handle, LValue);
      ReadValue(LValue);
      LMethod.Invoke(AValue, [LValue]);

      ReadEndElement();
    end;
  end;
end;

procedure TXmlReader.ReadEvent(var AValue: TValue);
var
  LEvent: PMethod;
  LMethod: TRttiMethod;
begin
  LEvent := AValue.GetReferenceToRawData();
  if FRoot.TryGetMethod(VarToStrDef(FCurrentNode.NodeValue, ''), LMethod) then
  begin
    LEvent.Data := FRoot;
    LEvent.Code := LMethod.CodeAddress;
  end;
end;

procedure TXmlReader.ReadObject(var AValue: TValue);
var
  LObject: TObject;
  LProperty: TRttiProperty;
  LValue: TValue;
  LField: TRttiField;

  function FindPropertyByElementName(AObject: TObject;
    const AElementName: string; out AProperty: TRttiProperty): Boolean;
  var
    LProperty: TRttiProperty;
    LAttribute: XmlElementAttribute;
  begin
    Result := False;
    for LProperty in AObject.GetProperties do
    begin
      if LProperty.TryGetAttributeOfType<XmlElementAttribute>(LAttribute)
        and SameText(LAttribute.ElementName, AElementName) then
      begin
        AProperty := LProperty;
        Result := True;
        Break;
      end;
    end;
  end;

  function FindFieldByElementName(AObject: TObject;
    const AElementName: string; out AField: TRttiField): Boolean;
  var
    LField: TRttiField;
    LAttribute: XmlElementAttribute;
  begin
    Result := False;
    for LField in AObject.GetFields do
    begin
      if LField.TryGetAttributeOfType<XmlElementAttribute>(LAttribute)
        and SameText(LAttribute.ElementName, AElementName) then
      begin
        AField := LField;
        Result := True;
        Break;
      end;
    end;
  end;

begin
  ReadEnumerable(AValue);

  LObject := AValue.AsObject;
  while IsStartElement do
  begin
    ReadStartElement();

    if (LObject.TryGetProperty(FCurrentNode.NodeName, LProperty)
      or FindPropertyByElementName(LObject, FCurrentNode.NodeName, LProperty))
      and (LProperty.IsWritable or LProperty.PropertyType.IsInstance) then
    begin
      LValue := LProperty.GetValue(LObject);
      ReadValue(LValue);
      if not LProperty.PropertyType.IsInstance then
      begin
        LProperty.SetValue(LObject, LValue);
      end;
    end
    else
    if LObject.TryGetField(FCurrentNode.NodeName, LField)
      or FindFieldByElementName(LObject, FCurrentNode.NodeName, LField) then
    begin
      LValue := LField.GetValue(LObject);
      ReadValue(LValue);
      if not LField.FieldType.IsInstance then
      begin
        LField.SetValue(LObject, LValue);
      end;
    end;

    ReadEndElement();
  end;
end;

procedure TXmlReader.ReadStartElement;
begin
  if Assigned(FCurrentNode) then
  begin
    FCurrentNode := FCurrentNode.ChildNodes[FIndex];
  end
  else
  begin
    FCurrentNode := FDocument.DocumentElement;
  end;
  FIndex := 0;
end;

procedure TXmlReader.ReadStartElement(const AName: string);
begin
  if IsStartElement(AName) then
  begin
    ReadStartElement();
  end
  else
  begin
    raise EXmlException.CreateFmt('Element "%s" not found', [AName]);
  end;
end;

procedure TXmlReader.ReadValue(var AValue: TValue);
begin
  if AValue.IsEmpty then
  begin
    CreateObject(AValue);
  end;

  case AValue.Kind of
    tkInteger, tkInt64:
    begin
      AValue := TValue.FromOrdinal(AValue.TypeInfo, StrToInt64(FCurrentNode.NodeValue));
    end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
    begin
      AValue := TValue.From<string>(VarToStrDef(FCurrentNode.NodeValue, ''));
    end;
    tkEnumeration:
    begin
      AValue := TValue.FromOrdinal(AValue.TypeInfo,
        GetEnumValue(AValue.TypeInfo, FCurrentNode.NodeValue));
    end;
    tkFloat:
    begin
      if AValue.IsDate then
      begin
        AValue := TValue.From<TDate>(StrToDate(FCurrentNode.NodeValue, XmlFormatSettings));
      end else
      if AValue.IsDateTime then
      begin
        AValue := TValue.From<TDateTime>(XMLTimeToDateTime(FCurrentNode.NodeValue));
      end else
      if AValue.IsTime then
      begin
        AValue := TValue.From<TTime>(XMLTimeToTime(FCurrentNode.NodeValue));
      end
      else
      begin
        FCurrentNode.NodeValue := FloatToStr(AValue.AsExtended, XmlFormatSettings);
      end;
    end;
    tkSet:
    begin
      TValue.Make(StringToSet(AValue.TypeInfo, FCurrentNode.NodeValue),
        AValue.TypeInfo, AValue);
    end;
    tkClass:
    begin
      ReadObject(AValue);
    end;
    tkMethod:
    begin
      ReadEvent(AValue);
    end;
  end;
end;

procedure TXmlReader.SetXml(const Value: string);
begin
  FDocument.XML.Text := Value;
  FDocument.Active := True;
  FCurrentNode := nil;
  FIndex := 0;
end;

end.
