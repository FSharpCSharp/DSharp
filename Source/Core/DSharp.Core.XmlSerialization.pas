unit DSharp.Core.XmlSerialization;

interface

uses
  SysUtils,
  Rtti;

type
  XmlIgnoreAttribute = class(TCustomAttribute)
  end;

  XmlElementAttribute = class(TCustomAttribute)
  private
    FElementName: string;
  public
    constructor Create(const AElementName: string);

    property ElementName: string read FElementName;
  end;

  IXmlReader = interface
    function GetXml: string;
    procedure SetXml(const Value: string);

    function IsStartElement(): Boolean; overload;
    function IsStartElement(const AName: string): Boolean; overload;
    procedure ReadStartElement; overload;
    procedure ReadStartElement(const AName: string); overload;
    procedure ReadEndElement; overload;
    procedure ReadValue(var AValue: TValue);

    property Xml: string read GetXml write SetXml;
  end;

  IXmlWriter = interface
    function GetXml: string;

    procedure WriteStartElement(const AName: string);
    procedure WriteEndElement(const AName: string);
    procedure WriteValue(const AValue: TValue);

    property Xml: string read GetXml;
  end;

  IXmlSerializer = interface
    function Deserialize(AReader: IXmlReader): TObject;
    procedure Serialize(AObject: TObject; AWriter: IXmlWriter);
  end;

  IXmlSerializable = interface
    procedure ReadXml(AReader: IXmlReader);
    procedure WriteXml(AWriter: IXmlWriter);
  end;

  EXmlException = class(Exception)
  end;

var
  XmlFormatSettings: TFormatSettings;

implementation

{ XmlElementAttribute }

constructor XmlElementAttribute.Create(const AElementName: string);
begin
  FElementName := AElementName;
end;

initialization
  XmlFormatSettings := TFormatSettings.Create();
  XmlFormatSettings.DateSeparator := '-';
  XmlFormatSettings.DecimalSeparator := '.';
  XmlFormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';
  XmlFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

end.
