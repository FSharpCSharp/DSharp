unit ShellViewModel;

interface

uses
  Classes,
  SysUtils,
  Interfaces,
  DSharp.Collections,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive;

type
  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  TShellViewModel = class(TConductorCollectionOneActive<IScreen>,
    IShellViewModel)
  strict private
    FExampleOne: IScreen;
    FExampleTwo: IScreen;
  public
    constructor Create(const AExampleOne: IExampleOneViewModel;
      const AExampleTwo: IExampleTwoViewModel);
    procedure ShowExampleOne;
    procedure ShowExampleTwo;
  end;

implementation

{ TShellViewModel }

constructor TShellViewModel.Create(const AExampleOne: IExampleOneViewModel;
  const AExampleTwo: IExampleTwoViewModel);
begin
  inherited Create();

  if not Assigned(AExampleOne) then
    raise EArgumentNilException.Create('AExampleOne');

  if not Assigned(AExampleTwo) then
    raise EArgumentNilException.Create('AExampleTwo');

  FExampleOne := AExampleOne;
  FExampleTwo := AExampleTwo;
end;

procedure TShellViewModel.ShowExampleOne;
begin
  ActivateItem(FExampleOne);
end;

procedure TShellViewModel.ShowExampleTwo;
begin
  ActivateItem(FExampleTwo);
end;

initialization

TShellViewModel.ClassName;

end.
