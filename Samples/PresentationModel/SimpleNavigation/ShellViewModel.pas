unit ShellViewModel;

interface

uses
  Interfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive;

type
  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  TShellViewModel = class(TConductorCollectionOneActive<IScreen>,
    IShellViewModel)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create; override;
    procedure ShowPageOne;
    procedure ShowPageTwo;
  end;

implementation

uses
  PageOneViewModel,
  PageTwoViewModel;

{ TShellViewModel }

constructor TShellViewModel.Create;
begin
  inherited;
  ShowPageOne;
end;

procedure TShellViewModel.ShowPageOne;
begin
  ActivateItem(TPageOneViewModel.Create);
end;

procedure TShellViewModel.ShowPageTwo;
begin
  ActivateItem(TPageTwoViewModel.Create);
end;

initialization

TShellViewModel.ClassName;

end.
