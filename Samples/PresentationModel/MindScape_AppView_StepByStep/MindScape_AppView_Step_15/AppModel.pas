unit AppModel;

interface

uses
  IniFiles,
  AppInterfaces;

type
  TAppModel = class(TInterfacedObject, IAppModel)
  strict private
    FCount: Integer;
    FIncrementValue: Integer;
  const
    SAppModel = 'AppModel';
    SCount = 'Count';
    SIncrementValue = 'IncrementValue';
  protected
    function CreateIniFile(): TIniFile; virtual;
    function GetCount(): Integer; virtual;
    function GetIncrementValue(): Integer; virtual;
    function GetIniFileName(): string; virtual;
    procedure SetCount(const Value: Integer); virtual;
    procedure SetIncrementValue(const Value: Integer); virtual;
  public
    constructor Create();
    destructor Destroy(); override;
    property Count: Integer read GetCount write SetCount;
    property IncrementValue: Integer read GetIncrementValue write SetIncrementValue;
    property IniFileName: string read GetIniFileName;
  end;

implementation

uses
  SysUtils,
  Spring;

constructor TAppModel.Create();
var
  IniFile: TIniFile;
begin
  inherited Create();
  IniFile := CreateIniFile();
  try
    Count := IniFile.ReadInteger(SAppModel, SCount, 0);
    IncrementValue := IniFile.ReadInteger(SAppModel, SIncrementValue, 0);
  finally
    IniFile.Free();
  end;
end;

destructor TAppModel.Destroy();
var
  IniFile: TIniFile;
begin
  IniFile := CreateIniFile();
  try
    IniFile.WriteInteger(SAppModel, SCount, Count);
    IniFile.WriteInteger(SAppModel, SIncrementValue, IncrementValue);
  finally
    IniFile.Free();
  end;
  inherited Destroy();
end;

function TAppModel.CreateIniFile(): TIniFile;
begin
  Result := TIniFile.Create(IniFileName);
end;

function TAppModel.GetCount(): Integer;
begin
  Result := FCount;
end;

function TAppModel.GetIncrementValue(): Integer;
begin
  Result := FIncrementValue;
end;

function TAppModel.GetIniFileName(): string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TAppModel.SetCount(const Value: Integer);
begin
  Guard.CheckRange((Value >= MinimumCount) and (Value <= MaximumCount), 'Value');
  if FCount <> Value then
    FCount := Value;
end;

procedure TAppModel.SetIncrementValue(const Value: Integer);
begin
  FIncrementValue := Value;
end;

initialization
  TAppModel.ClassName;
end.
