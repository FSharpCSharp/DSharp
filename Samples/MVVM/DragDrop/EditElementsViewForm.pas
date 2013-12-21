unit EditElementsViewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Grids, DSharp.Bindings.VCLControls,
  DSharp.Bindings, ActnList, System.Actions;

type
  TEditElementsView = class(TForm)
    AvailableElements: TListBox;
    btnAddElements: TButton;
    btnRemoveElements: TButton;
    SelectedElements: TListBox;
    BindingGroup1: TBindingGroup;
    ActionList1: TActionList;
    AddElements: TAction;
    RemoveElements: TAction;
    procedure AvailableElementsDblClick(Sender: TObject);
    procedure SelectedElementsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SelectedElementsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure AvailableElementsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SelectedElementsDblClick(Sender: TObject);
    procedure AvailableElementsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  end;

var
  EditElementsView: TEditElementsView;

implementation

{$R *.dfm}

procedure TEditElementsView.AvailableElementsDblClick(Sender: TObject);
begin
  AddElements.Execute;
end;

procedure TEditElementsView.AvailableElementsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  RemoveElements.Execute;
end;

procedure TEditElementsView.AvailableElementsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = SelectedElements;
end;

procedure TEditElementsView.SelectedElementsDblClick(Sender: TObject);
begin
  RemoveElements.Execute;
end;

procedure TEditElementsView.SelectedElementsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  AddElements.Execute;
end;

procedure TEditElementsView.SelectedElementsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = AvailableElements;
end;

end.
