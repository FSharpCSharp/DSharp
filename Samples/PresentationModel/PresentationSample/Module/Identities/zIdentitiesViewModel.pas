unit zIdentitiesViewModel;

interface

uses
  Classes,
  SysUtils,
  Forms,
  DSharp.PresentationModel,
  Interfaces;

type
  TIdentitiesViewModel = class(TScreen, IIdentitiesViewModel)
  public
    constructor Create; override;
    procedure ActionShowSelectedAccount(Sender: TObject);
  end;

implementation

uses
  zAccountViewModel,
  Graphics,
  zAccount;

{ TIdentitiesViewModel }

procedure TIdentitiesViewModel.ActionShowSelectedAccount(Sender: TObject);
var
  LSettings: IViewSettings;
  LAccount: TAccount;
begin
  LAccount := TAccount.Create;
  LAccount.Name := 'My simple account';

  LSettings := TViewSettings.Create;
  LSettings['BorderStyle'] := TValue.From(bsSizeToolWin);
  LSettings['Color'] := TValue.From(clSkyBlue);

  IoC.Get<IWindowManager>.ShowDialog(TAccountViewModel.Create.WithSubject
    (LAccount), nil, LSettings);

  LAccount.Free;
end;

constructor TIdentitiesViewModel.Create;
begin
  inherited Create;
  DisplayName := 'Hello';
end;

initialization

TIdentitiesViewModel.ClassName;

end.
