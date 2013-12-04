unit ModuleCreator;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Interfaces;

type
  /// <summary>
  /// TModuleCreator creates a view + viewmodel based on default template
  /// </summary>
  TModuleCreator = class(TInterfacedObject, ICreator)
  private
    FModuleName: string;
    FSettings: ISettings;
    procedure CreateView;
    procedure CreateViewDsgn;
    procedure CreateViewModel;
    function GetViewModelInterfaceCode: string;
    procedure RegisterViewModelInterface;
    function GetViewBaseClass: string;
    function GetViewModelBaseClass: string;
    procedure Make;
    property ModuleName: string read FModuleName;
    property ViewBaseClass: string read GetViewBaseClass;
    property ViewModelBaseClass: string read GetViewModelBaseClass;
  public
    constructor Create(ModuleName: string; Settings: ISettings);
  end;

implementation

uses
  TemplateEngine,
  IOUtils;

const
  SModuleViewModelInterfaceTemplate = 'ModuleViewModelInterfaceTemplate';
  SModuleViewModelTemplate = 'ModuleViewModelTemplate';
  SModuleViewDsgnTemplate = 'VclModuleViewDsgnTemplate';
  SModuleViewTemplate = 'VclModuleViewTemplate';
  SViewSuffix = 'View';
  SViewModelSuffix = 'ViewModel';
  SSeparator = '.';

constructor TModuleCreator.Create(ModuleName: string; Settings: ISettings);
begin
  FModuleName := Trim(ModuleName);
  FSettings := Settings;
end;

procedure TModuleCreator.CreateView;
var
  LVariables: TDictionary<string, string>;
  LTemplate: ITemplateEngine;
begin
  LVariables := TDictionary<string, string>.Create;
  try
    LVariables.Add('UnitName', ModuleName + SViewSuffix + ViewBaseClass);
    LVariables.Add('TypeName', ModuleName + SViewSuffix);
    LVariables.Add('BaseClass', ViewBaseClass);
    LTemplate := TTemplateEngine.Create(SModuleViewTemplate);
    LTemplate.Replace(LVariables);
    LTemplate.Save(FSettings.ViewFolder[ModuleName] + ModuleName + SViewSuffix + ViewBaseClass + '.pas');
  finally
    LVariables.Free;
  end;
end;

procedure TModuleCreator.CreateViewDsgn;
var
  LVariables: TDictionary<string, string>;
  LTemplate: ITemplateEngine;
begin
  LVariables := TDictionary<string, string>.Create;
  try
    LVariables.Add('UnitName', ModuleName + SViewSuffix + ViewBaseClass);
    LVariables.Add('TypeName', ModuleName + SViewSuffix);
    LTemplate := TTemplateEngine.Create(SModuleViewDsgnTemplate);
    LTemplate.Replace(LVariables);
    LTemplate.Save(FSettings.ViewFolder[ModuleName] + ModuleName + SViewSuffix + ViewBaseClass + '.dfm');
  finally
    LVariables.Free;
  end;
end;

procedure TModuleCreator.CreateViewModel;
var
  LVariables: TDictionary<string, string>;
  LTemplate: ITemplateEngine;
begin
  LVariables := TDictionary<string, string>.Create;
  try
    LVariables.Add('UnitName', ModuleName + SViewModelSuffix);
    LVariables.Add('TypeName', ModuleName + SViewModelSuffix);
    LVariables.Add('BaseClass', ViewModelBaseClass);
    LVariables.Add('InterfaceGuid', TGuid.NewGuid.ToString);
    LTemplate := TTemplateEngine.Create(SModuleViewModelTemplate);
    LTemplate.Replace(LVariables);
    LTemplate.Save(FSettings.ViewModelFolder[ModuleName] + ModuleName + SViewModelSuffix + '.pas');
  finally
    LVariables.Free;
  end;
end;

procedure TModuleCreator.RegisterViewModelInterface;
var
  LTemplate: ITemplateEngine;
begin
  // Ensure interfaces file exists
  if not TFile.Exists(FSettings.ApplicationInterfacesFileName) then
  begin
    // Create interfaces file
    Writeln('Creating ' + FSettings.ApplicationInterfacesFileName);
    LTemplate := TTemplateEngine.Create('InterfacesTemplate');
    LTemplate.Save(FSettings.ApplicationInterfacesFileName);
  end;

  // Create template from interfaces file
  LTemplate := TTemplateEngine.Create(FSettings.ApplicationInterfacesFileName);

  // Ensure interface is not registered
  if AnsiPos(ModuleName + SViewModelSuffix, LTemplate.ToString) > 0 then
  begin
    // Writeln('Skipping registration of ', ModuleName);
    Exit;
  end;

  // Register interface
  Writeln('Registering ', ModuleName);
  LTemplate.Replace('implementation', GetViewModelInterfaceCode + 'implementation', True);
  LTemplate.Save(FSettings.ApplicationFolder + FSettings.ApplicationInterfacesFileName);
end;

function TModuleCreator.GetViewBaseClass: string;
begin
  if SameText('Shell', ModuleName) then
  begin
    Result := 'Form'
  end
  else
  begin
    Result := 'Frame';
  end;
end;

function TModuleCreator.GetViewModelBaseClass: string;
begin
  if SameText('Shell', ModuleName) then
  begin
    Result := 'TConductorCollectionOneActive<IScreen>'
  end
  else
  begin
    Result := 'TScreen';
  end;
end;

function TModuleCreator.GetViewModelInterfaceCode: string;
var
  LVariables: TDictionary<string, string>;
  LTemplate: ITemplateEngine;
begin
  LVariables := TDictionary<string, string>.Create;
  try
    LVariables.Add('TypeName', ModuleName + SViewModelSuffix);
    LVariables.Add('InterfaceGuid', TGuid.NewGuid.ToString);
    LTemplate := TTemplateEngine.Create(SModuleViewModelInterfaceTemplate);
    LTemplate.Replace(LVariables);
    Result := LTemplate.ToString;
  finally
    LVariables.Free;
  end;
end;

procedure TModuleCreator.Make;
begin
  Writeln('Creating module ', ModuleName);
  CreateView;
  CreateViewDsgn;
  CreateViewModel;
  RegisterViewModelInterface;
end;

end.
