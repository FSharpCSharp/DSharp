program DragDropSample;

uses
  DSharp.PresentationModel.VCLApplication,
  DSharp.PresentationModel.ViewModelBinder,
  DSharp.PresentationModel.VCLConventionManager,
  Forms,
  EditElementsViewForm in 'EditElementsViewForm.pas' {EditElementsView} ,
  EditElementsViewModel in 'EditElementsViewModel.pas';

{$R *.res}

var
  EditElementsViewModel: TEditElementsViewModel;

procedure Wireup;
begin
  EditElementsViewModel := TEditElementsViewModel.Create(nil);

  // add some items
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);
  EditElementsViewModel.AvailableElements.Add(TObject.Create);

  // connect controls to viewmodel properties
  ViewModelBinder.Bind(EditElementsViewModel, EditElementsView, nil);
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEditElementsView, EditElementsView);
  Wireup;
  Application.Run;
  EditElementsViewModel.Free;

end.
