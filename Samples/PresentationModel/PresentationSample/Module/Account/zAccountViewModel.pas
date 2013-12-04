unit zAccountViewModel;

interface

uses
  Classes,
  SysUtils,
  DSharp.PresentationModel,
  Interfaces,
  DSharp.PresentationModel.ViewModelBase,
  zAccount;

type
  TAccountViewModel = class(TViewModel<TAccount>, IAccountViewModel)
  type
    {$SCOPEDENUMS ON }
    TActiveState = (Details, Authorizations, Groups);
    {$SCOPEDENUMS ON }
  private
    FActiveState: TActiveState;
    function GetAccountName: string;
    procedure SetActiveState(const Value: TActiveState);
  protected
    procedure OnInitialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property AccountName: string read GetAccountName;
    property ActiveState: TActiveState read FActiveState write SetActiveState;
    procedure ActionDetails(Sender: TObject);
    procedure ActionAuthorizations(Sender: TObject);
    procedure ActionGroups(Sender: TObject);
  end;

implementation

{ TAccountViewModel }

constructor TAccountViewModel.Create;
begin
  inherited;
  DisplayName := 'Hello';
end;

destructor TAccountViewModel.Destroy;
begin

  inherited;
end;

procedure TAccountViewModel.ActionDetails(Sender: TObject);
begin
  ActiveState := TActiveState.Details;
end;

procedure TAccountViewModel.ActionAuthorizations(Sender: TObject);
begin
  ActiveState := TActiveState.Authorizations;
end;

procedure TAccountViewModel.ActionGroups(Sender: TObject);
begin
  ActiveState := TActiveState.Groups;
end;

function TAccountViewModel.GetAccountName: string;
begin
  Result := Subject.Name;
end;

procedure TAccountViewModel.OnInitialize;
begin
  inherited;
  ActiveState := TActiveState.Details;
end;

procedure TAccountViewModel.SetActiveState(const Value: TActiveState);
begin
  if FActiveState <> Value then
  begin
    FActiveState := Value;
    // Notify active context changed
    NotifyOfPropertyChange('ActiveState');
  end;
end;

initialization

TAccountViewModel.ClassName;

end.
