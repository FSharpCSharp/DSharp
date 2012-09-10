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
{$IF COMPILERVERSION > 21}
  XmlFormatSettings := TFormatSettings.Create();
{$IFEND}
  XmlFormatSettings.DateSeparator := '-';
  XmlFormatSettings.DecimalSeparator := '.';
  XmlFormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';
  XmlFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

end.
