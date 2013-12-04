unit MyDialogViewModel;

interface

uses
  Classes,
  AppInterfaces,
  DSharp.PresentationModel;

type
  TMyDialogViewModel = class(TScreen, IMyDialogViewModel)
  strict private
    FValue: string;
  strict protected
    function GetValue: string; virtual;
    procedure SetValue(const Value: string); virtual;
  public
    procedure ActionCancel; virtual;
    procedure ActionOK; virtual;
    property Value: string read GetValue write SetValue;
  end;

implementation

procedure TMyDialogViewModel.ActionCancel;
begin
  TryClose(mrCancel);
end;

function TMyDialogViewModel.GetValue: string;
begin
  Result := FValue;
end;

procedure TMyDialogViewModel.ActionOK;
begin
  TryClose(mrOk);
end;

procedure TMyDialogViewModel.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    NotifyOfPropertyChange('Value');
  end;
end;

initialization

TMyDialogViewModel.ClassName;

end.
