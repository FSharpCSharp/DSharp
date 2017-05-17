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

unit DSharp.Core.XmlSerialization.XmlSerializer;

interface

uses
  DSharp.Core.XmlSerialization,
  TypInfo;

type
  TXmlSerializer = class(TInterfacedObject, IXmlSerializer)
  public
    function Deserialize(reader: IXmlReader): TObject; virtual;
    procedure Serialize(instance: TObject; writer: IXmlWriter); virtual;
  end;

  TXmlSerializer<T: class, constructor> = class(TXmlSerializer)
  private
    function GetTypeName: string;
  public
    function Deserialize(reader: IXmlReader): TObject; override;
    procedure Serialize(instance: TObject; writer: IXmlWriter); override;
  end;

implementation

uses
  DSharp.Core.Reflection,
  Rtti;

{ TXmlSerializer }

function TXmlSerializer.Deserialize(reader: IXmlReader): TObject;
var
  value: TValue;
begin
  reader.ReadStartElement;

  reader.ReadValue(value);
  if value.IsObject then
    Result := value.AsObject
  else
    Result := nil;

  reader.ReadEndElement;
end;

procedure TXmlSerializer.Serialize(instance: TObject; writer: IXmlWriter);
begin
  writer.WriteStartElement(instance.ClassName);
  writer.WriteValue(TValue.From<TObject>(instance));
  writer.WriteEndElement(instance.ClassName);
end;

{ TXmlSerializer<T> }

function TXmlSerializer<T>.Deserialize(reader: IXmlReader): TObject;
var
  value: TValue;
begin
  reader.ReadStartElement;
  Result := T.Create;
  value := TValue.From<T>(Result);
  reader.ReadValue(value);
  reader.ReadEndElement;
end;

function TXmlSerializer<T>.GetTypeName: string;
var
  rttiType: TRttiType;
  rootAttribute: XmlRootAttribute;
begin
  rttiType := GetRttiType(TypeInfo(T));
  if rttiType.TryGetCustomAttribute<XmlRootAttribute>(rootAttribute) then
    Exit(rootAttribute.ElementName);
  if rttiType.IsPublicType then
    Result := rttiType.QualifiedName
  else
    Result := rttiType.Name;
  if rttiType.IsGenericTypeDefinition then
  begin
    Result := Copy(Result, 1, Pos('<', Result) - 1) + 'Of';
    // keep it simple for now -> will move to reflection in the future
    for rttiType in rttiType.GetGenericArguments do
      Result := Result + rttiType.Name;
  end;
end;

procedure TXmlSerializer<T>.Serialize(instance: TObject; writer: IXmlWriter);
var
  typeName: string;
begin
  typeName := GetTypeName;
  writer.WriteStartElement(typeName);
  writer.WriteValue(TValue.From<T>(instance));
  writer.WriteEndElement(typeName);
end;

end.
