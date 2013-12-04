unit AppModel;

interface

uses
  AppInterfaces,
  IniFiles;

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
    property IncrementValue: Integer read GetIncrementValue
      write SetIncrementValue;
    property IniFileName: string read GetIniFileName;
  end;

implementation

uses
  SysUtils;

constructor TAppModel.Create();
var
  IniFile: TIniFile;
begin
  inherited;
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
  inherited;
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
  if FCount <> Value then
  begin
    if (Value < MinimumCount) or (Value > MaximumCount) then
      raise ERangeError.CreateFmt('Count value %d out of range %d..%d',
        [Value, MinimumCount, MaximumCount]);
    FCount := Value;
  end;
end;

procedure TAppModel.SetIncrementValue(const Value: Integer);
begin
  FIncrementValue := Value;
end;

initialization

TAppModel.ClassName;

end.
