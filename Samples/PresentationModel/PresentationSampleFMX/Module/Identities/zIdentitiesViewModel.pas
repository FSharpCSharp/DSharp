unit zIdentitiesViewModel;

interface

uses
  Classes,
  SysUtils,
  DSharp.PresentationModel,
  Interfaces;

type
  TIdentitiesViewModel = class(TScreen, IIdentitiesViewModel)
  public
    constructor Create; override;
    procedure ActionShowSelectedAccount(Sender: TObject);
  end;

implementation

{ TIdentitiesViewModel }

procedure TIdentitiesViewModel.ActionShowSelectedAccount(Sender: TObject);
begin
  // IoC.Get<IWindowManager>.ShowDialog(TAccountViewModel.Create);
  IoC.Get<IWindowManager>.ShowMessage('Hi');
end;

constructor TIdentitiesViewModel.Create;
begin
  inherited Create;
  DisplayName := 'Hello';
end;

end.
