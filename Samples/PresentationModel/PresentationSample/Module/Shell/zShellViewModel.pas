unit zShellViewModel;

interface

uses
  SysUtils,
  Classes,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionAllActive,
  Interfaces,
  Spring.Services;

type
  TShellViewModel = class(TConductorCollectionAllActive<IScreen>, IShell)
  private
    FMain: IScreen;
  public
    constructor Create; override;
    property Main: IScreen read FMain;
  end;

implementation

uses
  zMainViewModel;

constructor TShellViewModel.Create;
begin
  inherited Create;
  OpenPublicItems := True;
  FMain := TMainViewModel.Create;
end;

initialization

TShellViewModel.ClassName;

end.
