unit zMainViewModel;

interface

uses
  SysUtils,
  Classes,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive,
  Interfaces,
  Spring.Services;

type
  TMainViewModel = class(TConductorCollectionOneActive<IScreen>, IShell)
  private
    FAccounts: IScreen;
    FHeader: IScreen;
    FIdentities: IScreen;
    procedure Log(Value: string);
  protected
    procedure OnInitialize; override;
  public
    constructor Create; override;
    procedure ActionAccount;
    procedure ActionClose;
    procedure ActionDocuments;
    procedure ActionExport;
    procedure ActionIdentities;
    procedure ActionInfo;
    procedure ActionNew;
    procedure ActionOpen;
    procedure ActionOptions;
    procedure ActionPrint;
    procedure ActionSave;
    procedure ActionSaveAs;
    procedure ActionShare;
    property Header: IScreen read FHeader;
  end;

implementation

uses
  zDocumentsViewModel,
  zIdentitiesViewModel,
  zHeaderViewModel,
  zSomeViewModel;

constructor TMainViewModel.Create;
begin
  inherited Create;
  DisplayName := 'Main';
  FHeader := THeaderViewModel.Create;
  FAccounts := TDocumentsViewModel.Create;
  FIdentities := TIdentitiesViewModel.Create;
  Items.Add(FAccounts);
  Items.Add(FIdentities);
end;

procedure TMainViewModel.ActionAccount;
begin
  Log('Account');
end;

procedure TMainViewModel.ActionClose;
begin
  Log('Close');
end;

procedure TMainViewModel.ActionDocuments;
begin
  (FHeader as IHaveDisplayName).DisplayName := 'Documents';
  ActivateItem(FAccounts);
end;

procedure TMainViewModel.ActionExport;
begin
  Log('Export');
end;

procedure TMainViewModel.ActionIdentities;
begin
  (FHeader as IHaveDisplayName).DisplayName := 'Identities';
  ActivateItem(FIdentities);
end;

procedure TMainViewModel.ActionInfo;
begin
  Log('Info');
end;

procedure TMainViewModel.ActionNew;
begin
  Log('New');
end;

procedure TMainViewModel.ActionOpen;
begin
  Log('Open');
end;

procedure TMainViewModel.ActionOptions;
begin
  Log('Options');
end;

procedure TMainViewModel.ActionPrint;
begin
  Log('Print');
end;

procedure TMainViewModel.ActionSave;
begin
  Log('Save');
end;

procedure TMainViewModel.ActionSaveAs;
begin
  Log('Save As');
end;

procedure TMainViewModel.ActionShare;
begin
  Log('Share');
end;

procedure TMainViewModel.Log(Value: string);
var
  LModel: IScreen;
begin
  (FHeader as IHaveDisplayName).DisplayName := Value;

  LModel := TSomeViewModel.Create;
  Items.Add(LModel);
  (LModel as IHaveDisplayName).DisplayName := Value +
    ' action is not yet implemented.';
  ActivateItem(LModel);
end;

procedure TMainViewModel.OnInitialize;
begin
  inherited;
  ActionDocuments;
end;

initialization

TMainViewModel.ClassName;

end.
