unit DSharp.Core.XmlSerialization.XmlSerializer;

interface

uses
  DSharp.Core.XmlSerialization;

type
  TXmlSerializer = class(TInterfacedObject, IXmlSerializer)
  public
    function Deserialize(AReader: IXmlReader): TObject;
    procedure Serialize(AObject: TObject; AWriter: IXmlWriter);
  end;

implementation

uses
  Rtti;

{ TXmlSerializer }

function TXmlSerializer.Deserialize(AReader: IXmlReader): TObject;
var
  LValue: TValue;
begin
  AReader.ReadStartElement();

  AReader.ReadValue(LValue);
  if LValue.IsObject then
  begin
    Result := LValue.AsObject();
  end
  else
  begin
    Result := nil;
  end;

  AReader.ReadEndElement();
end;

procedure TXmlSerializer.Serialize(AObject: TObject; AWriter: IXmlWriter);
begin
  AWriter.WriteStartElement(AObject.ClassName);

  AWriter.WriteValue(TValue.From<TObject>(AObject));

  AWriter.WriteEndElement(AObject.ClassName);
end;

end.
