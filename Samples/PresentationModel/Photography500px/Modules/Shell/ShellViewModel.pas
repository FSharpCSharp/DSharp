unit ShellViewModel;

interface

uses
  Classes,
  SysUtils,
  Interfaces,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive;

type

  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  [PartCreationPolicy(cpShared)]
  TShellViewModel = class(TConductorCollectionOneActive<IScreen>,
    IShellViewModel)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(Gallery: IGalleryViewModel);
  end;

implementation

{ TShellViewModel }

constructor TShellViewModel.Create(Gallery: IGalleryViewModel);
begin
  inherited Create;

  ActivateItem(Gallery);

  {
    Execute.OnBackgroundThread(
    procedure
    begin
    Sleep(2000);
    Beep;
    end);
  }
end;

initialization

TShellViewModel.ClassName;

end.
