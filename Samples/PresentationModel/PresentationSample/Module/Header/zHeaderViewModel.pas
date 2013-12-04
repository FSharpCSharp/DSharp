unit zHeaderViewModel;

interface

uses
  Classes,
  SysUtils,
  Forms,
  DSharp.PresentationModel,
  Interfaces;

type
  THeaderViewModel = class(TScreen, IHeaderViewModel)
  public
    constructor Create; override;
  end;

implementation

constructor THeaderViewModel.Create;
begin
  inherited Create;
  DisplayName := 'Header';
end;

initialization

THeaderViewModel.ClassName;

end.
