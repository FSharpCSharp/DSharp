unit TemplateEngine;

interface

uses
  Classes,
  SysUtils,
  Windows,
  Generics.Collections;

type
  ITemplateEngine = interface
    ['{7E2CE170-AEC3-4188-966A-DB573751F747}']
    function Replace(OldValue: string; NewValue: string; Explicit: Boolean = False): ITemplateEngine; overload;
    function Replace(Variables: TDictionary<string, string>): ITemplateEngine; overload;
    procedure Save(FileName: string; Encoding: TEncoding = nil); overload;
    procedure Save(Stream: TStream; Encoding: TEncoding = nil); overload;
    function ToString: string;
  end;

  TTemplateEngine = class(TInterfacedObject, ITemplateEngine)
  private
    FBuilder: TStringBuilder;
    FDefaultEncoding: TEncoding;
    procedure EnsureDirectory(Path: string);
    function LoadTemplate(TemplateName: string; out TemplateValue: TBytes): Boolean;
  public
    constructor Create(const TemplateName: string);
    destructor Destroy; override;
    function Replace(OldValue: string; NewValue: string; Explicit: Boolean = False): ITemplateEngine; overload; virtual;
    function Replace(Variables: TDictionary<string, string>): ITemplateEngine; overload;
    procedure Save(FileName: string; Encoding: TEncoding = nil); overload;
    procedure Save(Stream: TStream; Encoding: TEncoding = nil); overload;
    function ToString: string; override;
  end;

implementation

uses
  IOUtils;

constructor TTemplateEngine.Create(const TemplateName: string);
var
  LTemplate: string;
  LValue: TBytes;
  LPreambleLength: Integer;
begin
  // Get template
  if not LoadTemplate(TemplateName, LValue) then
    raise Exception.CreateFmt('Template %s not found!', [TemplateName]);

  // Detect template input encoding
  LPreambleLength := TEncoding.GetBufferEncoding(LValue, FDefaultEncoding);

  if FDefaultEncoding = nil then
    FDefaultEncoding := TEncoding.UTF8;

  // Decode template into unicode string
  LTemplate := FDefaultEncoding.GetString(LValue, LPreambleLength, Length(LValue) - LPreambleLength);

  // Load template
  FBuilder := TStringBuilder.Create(LTemplate);
end;

destructor TTemplateEngine.Destroy;
begin
  FreeAndNil(FBuilder);
  inherited;
end;

function TTemplateEngine.LoadTemplate(TemplateName: string; out TemplateValue: TBytes): Boolean;
var
  LStream: TStream;
begin
  // Find template resource
  if FileExists(TemplateName) then
  begin
    LStream := TFileStream.Create(TemplateName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(TemplateValue, LStream.Size);
      LStream.Position := 0;
      LStream.ReadBuffer(TemplateValue[0], LStream.Size);
      Result := True;
    finally
      FreeAndNil(LStream);
    end;
  end
  else
  begin
    LStream := TResourceStream.Create(HInstance, TemplateName, RT_RCDATA);
    try
      SetLength(TemplateValue, LStream.Size);
      LStream.Position := 0;
      LStream.ReadBuffer(TemplateValue[0], LStream.Size);
      Result := True;
    finally
      FreeAndNil(LStream);
    end;
  end;
end;

function TTemplateEngine.Replace(OldValue: string; NewValue: string; Explicit: Boolean = False): ITemplateEngine;
begin
  if not Explicit then
    OldValue := '<%' + OldValue + '%>';
  FBuilder.Replace(OldValue, NewValue);
  Result := Self;
end;

function TTemplateEngine.Replace(Variables: TDictionary<string, string>): ITemplateEngine;
var
  LPair: TPair<string, string>;
begin
  for LPair in Variables do
  begin
    Replace(LPair.Key, LPair.Value);
  end;
  Result := Self;
end;

procedure TTemplateEngine.EnsureDirectory(Path: string);
begin
  Path := ExtractFileDir(Path);
  if (Path <> '') and (not TDirectory.Exists(Path)) then
    TDirectory.CreateDirectory(Path);
end;

procedure TTemplateEngine.Save(FileName: string; Encoding: TEncoding = nil);
var
  LStream: TFileStream;
begin
  EnsureDirectory(FileName);
  LStream := TFileStream.Create(FileName, fmCreate, fmShareExclusive);
  try
    Save(LStream, Encoding);
  finally
    LStream.Free;
  end;
end;

procedure TTemplateEngine.Save(Stream: TStream; Encoding: TEncoding = nil);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding <> FDefaultEncoding then
    Encoding := FDefaultEncoding;

  Buffer := FDefaultEncoding.GetBytes(FBuilder.ToString);

  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stream.WriteBuffer(Preamble[0], Length(Preamble));

  Stream.WriteBuffer(Buffer[0], Length(Buffer));
end;

function TTemplateEngine.ToString: string;
begin
  Result := FBuilder.ToString;
end;

end.
