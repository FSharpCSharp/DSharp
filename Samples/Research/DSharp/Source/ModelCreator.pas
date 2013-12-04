unit ModelCreator;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Interfaces;

type
  /// <summary>
  /// TModelCreator creates a model based on default template
  /// </summary>
  TModelCreator = class(TInterfacedObject, ICreator)
  private
    FModelName: string;
    FSettings: ISettings;
    procedure CreateModel;
    procedure Make;
    property ModelName: string read FModelName;
  public
    constructor Create(ModelName: string; Settings: ISettings);
  end;

implementation

uses
  TemplateEngine,
  IOUtils;

const
  SModelTemplate = 'ModelTemplate';

constructor TModelCreator.Create(ModelName: string; Settings: ISettings);
begin
  FModelName := Trim(ModelName);
  FSettings := Settings;
end;

procedure TModelCreator.CreateModel;
var
  LVariables: TDictionary<string, string>;
  LTemplate: ITemplateEngine;
begin
  LVariables := TDictionary<string, string>.Create;
  try
    LVariables.Add('UnitName', ModelName);
    LVariables.Add('TypeName', ModelName);
    LTemplate := TTemplateEngine.Create(SModelTemplate);
    LTemplate.Replace(LVariables);
    LTemplate.Save(FSettings.ModelFolder + ModelName + '.pas');
  finally
    LVariables.Free;
  end;
end;

procedure TModelCreator.Make;
begin
  Writeln('Creating model ', ModelName);
  CreateModel;
end;

end.
