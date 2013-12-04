unit ShellViewModel;

interface

uses
  SysUtils,
  DSharp.PresentationModel,
  DSharp.PresentationModel.Conductor,
  Interfaces;

type

  ///	<summary>
  ///	  Implementation of <see cref="IShellViewModel" />
  ///	</summary>
  [PartCreationPolicy(TCreationPolicy.cpShared)]
  TShellViewModel = class(TConductor<IScreen>, IShellViewModel)
  private
    FFirstScreen: IScreen;
  protected
    procedure OnInitialize; override;
  public
    constructor Create(FirstScreen: ISearchViewModel);
    procedure Back;
  end;

implementation

uses
  SearchViewModel;

{ TShellViewModel }

constructor TShellViewModel.Create(FirstScreen: ISearchViewModel);
begin
  inherited Create;
  FFirstScreen := FirstScreen;
end;

procedure TShellViewModel.Back;
begin
  ActivateItem(FFirstScreen);
end;

procedure TShellViewModel.OnInitialize;
begin
  ActivateItem(FFirstScreen);
  inherited;
end;

initialization

TShellViewModel.ClassName;

end.
