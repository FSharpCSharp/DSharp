unit DetailViewModel;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel.ViewModelBase,
  Interfaces;

type
  TDetailViewModel = class(TViewModelBase, IDetailViewModel)

  end;

implementation

initialization
  TDetailViewModel.ClassName;

end.
