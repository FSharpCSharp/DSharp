unit Settings;

interface

uses
  Classes,
  SysUtils,
  Interfaces;

type

  /// <summary>
  /// Implementation of <see cref = "ISettings" />
  /// </summary>
  TSettings = class(TInterfacedObject, ISettings)
  private
    FApplicationFolder: string;
    FApplicationInterfacesFileName: string;
    FApplicationName: string;
    FApplicationStructure: TApplicationStructure;
    FModelFolder: string;
    FModulesFolder: string;
    function GetApplicationFolder: string;
    function GetApplicationInterfacesFileName: string;
    function GetApplicationName: string;
    function GetApplicationStructure: TApplicationStructure;
    function GetModelFolder: string;
    function GetViewFolder(Name: string): string;
    function GetViewModelFolder(Name: string): string;
  public
    constructor Create;
  end;

implementation

uses
  Windows,
  IOUtils;

constructor TSettings.Create;
begin
  // Init defaults
  FApplicationName := 'ExampleApplication';
  FApplicationFolder := IncludeTrailingPathDelimiter(TDirectory.GetCurrentDirectory);
  FApplicationStructure := TApplicationStructure.Custom;
  FModelFolder := FApplicationFolder;
  FModulesFolder := FApplicationFolder;
  FApplicationInterfacesFileName := 'Interfaces.pas';

  // Set default model folder
  if TDirectory.Exists(FApplicationFolder + 'Model') then
  begin
    FModelFolder := IncludeTrailingPathDelimiter(FApplicationFolder + 'Model');
  end;

  // Determine if application uses views + viewmodels folder structure
  if TDirectory.Exists(FApplicationFolder + 'Views') then
    if TDirectory.Exists(FApplicationFolder + 'ViewModels') then
      FApplicationStructure := TApplicationStructure.ViewModels;

  // Determine if application uses features/modules folder structure
  if TDirectory.Exists(FApplicationFolder + 'Modules') then
  begin
    FApplicationStructure := TApplicationStructure.Features;
    FModulesFolder := IncludeTrailingPathDelimiter(FApplicationFolder + 'Modules');
  end;
end;

function TSettings.GetApplicationFolder: string;
begin
  Result := FApplicationFolder;
end;

function TSettings.GetApplicationInterfacesFileName: string;
begin
  Result := FApplicationInterfacesFileName;
end;

function TSettings.GetApplicationName: string;
begin
  Result := FApplicationName;
end;

function TSettings.GetApplicationStructure: TApplicationStructure;
begin
  Result := FApplicationStructure;
end;

function TSettings.GetModelFolder: string;
begin
  Result := FModelFolder;
end;

function TSettings.GetViewFolder(Name: string): string;
begin
  case FApplicationStructure of
    TApplicationStructure.Custom:
      Result := FApplicationFolder;
    TApplicationStructure.Features:
      Result := FModulesFolder + Name + PathDelim;
    TApplicationStructure.ViewModels:
      Result := FApplicationFolder + 'Views' + PathDelim;
  end;
end;

function TSettings.GetViewModelFolder(Name: string): string;
begin
  case FApplicationStructure of
    TApplicationStructure.Custom:
      Result := FApplicationFolder;
    TApplicationStructure.Features:
      Result := FModulesFolder + Name + PathDelim;
    TApplicationStructure.ViewModels:
      Result := FApplicationFolder + 'ViewModels' + PathDelim;
  end;
end;

end.
