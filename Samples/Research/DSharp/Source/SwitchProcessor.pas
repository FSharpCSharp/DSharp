unit SwitchProcessor;

interface

uses
  Classes,
  SysUtils,
  Settings,
  Interfaces;

type
  /// <summary>
  /// TSwitchProcessor handles command line switches
  /// </summary>
  TSwitchProcessor = class
  private
    class var FSettings: ISettings;
  public
    class constructor Create;
    class procedure ProcessApplication(ApplicationName: string);
    class procedure ProcessModules(ModuleName: string);
    class procedure ProcessModels(ModelName: string);
  end;

implementation

uses
  ApplicationCreator,
  ModuleCreator,
  ModelCreator,
  StrUtils;

{ TSwitchProcessor }

class constructor TSwitchProcessor.Create;
begin
  FSettings := TSettings.Create;
end;

class procedure TSwitchProcessor.ProcessApplication(ApplicationName: string);
var
  LCreator: ICreator;
begin
  LCreator := TApplicationCreator.Create(ApplicationName, FSettings);
  LCreator.Make;
end;

class procedure TSwitchProcessor.ProcessModules(ModuleName: string);
var
  LCreator: ICreator;
  LName: string;
begin
  for LName in SplitString(ModuleName, ',') do
  begin
    LCreator := TModuleCreator.Create(LName, FSettings);
    LCreator.Make;
  end;
end;

class procedure TSwitchProcessor.ProcessModels(ModelName: string);
var
  LCreator: ICreator;
  LName: string;
begin
  for LName in SplitString(ModelName, ',') do
  begin
    LCreator := TModelCreator.Create(LName, FSettings);
    LCreator.Make;
  end;
end;

end.
