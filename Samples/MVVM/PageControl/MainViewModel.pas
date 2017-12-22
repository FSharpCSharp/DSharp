unit MainViewModel;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.ViewModelBase,
  Interfaces,
  Spring.Collections,
  Spring.Container.Common;

type
  [PartCreationPolicy(cpShared)]
  TMainViewModel = class(TViewModelBase, IMainViewModel)
  private
    fContent: IList<IDetailViewModel>;
  public
    [Inject]
    constructor Create; override;
    procedure AddDetail;
    property Content: IList<IDetailViewModel> read fContent;
  end;

implementation

uses
  DetailViewModel;

{ TMainViewModel }

procedure TMainViewModel.AddDetail;
begin
  fContent.Add(TDetailViewModel.Create);
end;

constructor TMainViewModel.Create;
begin
  inherited;
  fContent := TCollections.CreateList<IDetailViewModel>;
end;

initialization
  TMainViewModel.ClassName;

end.
