unit EditElementsViewModel;

interface

uses
  Spring.Collections;

type
  TEditElementsViewModel = class
  private
    FAvailableElements: IObjectList;
    FSelectedElements: IObjectList;
    FCurrentAvailableElement: TObject;
    FCurrentSelectedElement: TObject;
  public
    constructor Create;

    procedure AddElements(Sender: TObject);
    procedure RemoveElements(Sender: TObject);

    property AvailableElements: IObjectList read FAvailableElements;
    property SelectedElements: IObjectList read FSelectedElements;

    property CurrentAvailableElement: TObject
      read FCurrentAvailableElement write FCurrentAvailableElement;
    property CurrentSelectedElement: TObject
      read FCurrentSelectedElement write FCurrentSelectedElement;
  end;

implementation

{ TEditElementsViewModel }

constructor TEditElementsViewModel.Create;
begin
  FAvailableElements := TCollections.CreateList<TObject> as IObjectList;
  FSelectedElements := TCollections.CreateList<TObject> as IObjectList;
end;

procedure TEditElementsViewModel.AddElements(Sender: TObject);
begin
  if Assigned(FCurrentAvailableElement) then
  begin
    FSelectedElements.Add(FCurrentAvailableElement);
    FAvailableElements.Remove(FCurrentAvailableElement);
  end;
end;

procedure TEditElementsViewModel.RemoveElements(Sender: TObject);
begin
  if Assigned(FCurrentSelectedElement) then
  begin
    FAvailableElements.Add(FCurrentSelectedElement);
    FSelectedElements.Remove(FCurrentSelectedElement);
  end;
end;

end.
