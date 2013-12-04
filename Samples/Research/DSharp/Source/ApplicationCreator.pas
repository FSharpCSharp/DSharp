unit ApplicationCreator;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Interfaces;

type
  /// <summary>
  /// TApplicationCreator creates an application based on default template
  /// </summary>
  TApplicationCreator = class(TInterfacedObject, ICreator)
  private
    FApplicationName: string;
    FSettings: ISettings;
    procedure CreateApplication;
    procedure Make;
    property ApplicationName: string read FApplicationName;
  public
    constructor Create(ApplicationName: string; Settings: ISettings);
  end;

implementation

uses
  TemplateEngine;

const
  SApplicationTemplate = 'VclApplicationTemplate';

constructor TApplicationCreator.Create(ApplicationName: string; Settings: ISettings);
begin
  FApplicationName := Trim(ApplicationName);
  FSettings := Settings;
end;

procedure TApplicationCreator.CreateApplication;
var
  LVariables: TDictionary<string, string>;
  LTemplate: ITemplateEngine;
begin
  LVariables := TDictionary<string, string>.Create;
  try
    LVariables.Add('ApplicationName', ApplicationName);
    LTemplate := TTemplateEngine.Create(SApplicationTemplate);
    LTemplate.Replace(LVariables);
    LTemplate.Save(FSettings.ApplicationFolder + ApplicationName + '.dpr');
  finally
    LVariables.Free;
  end;
end;

procedure TApplicationCreator.Make;
begin
  Writeln('Creating application ', ApplicationName);
  CreateApplication;
end;

end.
