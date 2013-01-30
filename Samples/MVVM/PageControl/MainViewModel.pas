unit MainViewModel;

interface

uses
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.ViewModelBase,
  Interfaces;

type
  [PartCreationPolicy(cpShared)]
  TMainViewModel = class(TViewModelBase, IMainViewModel)
  private
    fContent: IList<IDetailViewModel>;
  public
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
  fContent := TList<IDetailViewModel>.Create;
end;

initialization
  TMainViewModel.ClassName;

end.
