unit EditElementsViewModel;

interface

uses
  DSharp.Collections;

type
  TEditElementsViewModel = class
  private
    FAvailableElements: IList;
    FSelectedElements: IList;
    FCurrentAvailableElement: TObject;
    FCurrentSelectedElement: TObject;
  public
    constructor Create;

    procedure AddElements(Sender: TObject);
    procedure RemoveElements(Sender: TObject);

    property AvailableElements: IList read FAvailableElements;
    property SelectedElements: IList read FSelectedElements;

    property CurrentAvailableElement: TObject
      read FCurrentAvailableElement write FCurrentAvailableElement;
    property CurrentSelectedElement: TObject
      read FCurrentSelectedElement write FCurrentSelectedElement;
  end;

implementation

{ TEditElementsViewModel }

constructor TEditElementsViewModel.Create;
begin
  FAvailableElements := TList<TObject>.Create;
  FSelectedElements := TList<TObject>.Create;
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
