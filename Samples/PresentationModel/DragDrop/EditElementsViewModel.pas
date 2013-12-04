unit EditElementsViewModel;

interface

uses
  Classes,
  DSharp.Collections;

type
  TEditElementsViewModel = class(TComponent)
  private
    FAvailableElements: IList;
    FSelectedElements: IList;
    FCurrentAvailableElement: TObject;
    FCurrentSelectedElement: TObject;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddElements(Sender: TObject);
    procedure RemoveElements(Sender: TObject);

    property AvailableElements: IList read FAvailableElements;
    property SelectedElements: IList read FSelectedElements;

    property CurrentAvailableElement: TObject read FCurrentAvailableElement
      write FCurrentAvailableElement;
    property CurrentSelectedElement: TObject read FCurrentSelectedElement
      write FCurrentSelectedElement;
  end;

implementation

{ TEditElementsViewModel }

constructor TEditElementsViewModel.Create(AOwner: TComponent);
begin
  inherited;
  FAvailableElements := TObjectList<TObject>.Create;
  FSelectedElements := TObjectList<TObject>.Create;
end;

procedure TEditElementsViewModel.AddElements(Sender: TObject);
begin
  if Assigned(FCurrentAvailableElement) then
  begin
    FSelectedElements.Add(FCurrentAvailableElement);
    FAvailableElements.Extract(FCurrentAvailableElement);
  end;
end;

procedure TEditElementsViewModel.RemoveElements(Sender: TObject);
begin
  if Assigned(FCurrentSelectedElement) then
  begin
    FAvailableElements.Add(FCurrentSelectedElement);
    FSelectedElements.Extract(FCurrentSelectedElement);
  end;
end;

end.
