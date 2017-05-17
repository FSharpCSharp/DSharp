unit VirtualTreeviewSample.Templates;

interface

uses
  DSharp.Core.DataTemplates;

type
  TFolderTemplate = class(TDataTemplate)
  public
    function GetItem(const Item: TObject; const Index: Integer): TObject; override;
    function GetItemCount(const Item: TObject): Integer; override;
    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetTemplateDataClass: TClass; override;
  end;

  TFileTemplate = class(TDataTemplate)
  public
    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetTemplateDataClass: TClass; override;
  end;

implementation

{ TFolderTemplate }

function TFolderTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  Result := TFolder(Item).Files[Index];
end;

function TFolderTemplate.GetItemCount(const Item: TObject): Integer;
begin
  Result := TFolder(Item).Files.Count; // containing subfolders in that list as well
end;

function TFolderTemplate.GetTemplateDataClass: TClass;
begin
  Result := TFolder;
end;

function TFolderTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: Result := TFolder(Item).Name;
  end;
end;

{ TFileTemplate }

function TFileTemplate.GetTemplateDataClass: TClass;
begin
  Result := TFile;
end;

function TFileTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: Result := TFile(Item).Name;
    1: Result := DateTimeToStr(TFile(Item).ChangeDate);
    2: Result := IntToStr(TFile(Item).Size);
  end;
end;

end.
