unit zShellViewModel;

interface

uses
  SysUtils,
  Classes,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive,
  Interfaces,
  Spring.Services;

type
  TShellViewModel = class(TConductorCollectionOneActive<IScreen>, IShell)
  private
    FAccounts: IScreen;
    FIdentities: IScreen;
  protected
    procedure OnInitialize(); override;
  public
    constructor Create; override;
    procedure ActionShowDocuments(Sender: TObject);
    procedure ActionShowIdentities(Sender: TObject);
    procedure HelloButton(Sender: TObject);
  end;

implementation

uses
  zDocumentsViewModel,
  zIdentitiesViewModel;

constructor TShellViewModel.Create;
begin
  inherited Create;
  FAccounts := TDocumentsViewModel.Create;
  FIdentities := TIdentitiesViewModel.Create;
  Items.Add(FAccounts);
  Items.Add(FIdentities);
end;

procedure TShellViewModel.HelloButton(Sender: TObject);
begin
  IoC.Get<IWindowManager>.ShowMessage('Hello!')
end;

procedure TShellViewModel.ActionShowDocuments(Sender: TObject);
begin
  ActivateItem(FAccounts);
end;

procedure TShellViewModel.ActionShowIdentities(Sender: TObject);
begin
  ActivateItem(FIdentities);
end;

procedure TShellViewModel.OnInitialize;
begin
  inherited;
  ActivateItem(FAccounts);
end;

initialization

TShellViewModel.ClassName;

end.
