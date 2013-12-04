unit TaskViewModel;

interface

uses
  Classes,
  SysUtils,
  {$IF CompilerVersion > 22}
  UITypes,
  {$ELSE}
  Dialogs,
  Forms,
  {$IFEND}
  Interfaces,
  DSharp.PresentationModel;

type
  {$SCOPEDENUMS ON}
  TMode = (New, View, Edit);
  {$SCOPEDENUMS OFF}

  ///	<summary>
  ///	  Implementation of <see cref="ITaskViewModel" />
  ///	</summary>
  // [PartCreationPolicy(cpShared)]
  TTaskViewModel = class(TScreen, ITaskViewModel)
  strict private
    FDescription: string;
    FIsCompleted: Boolean;
    FIsReadOnly: Boolean;
    FMode: TMode;
    function GetAddVisible: Boolean;
    procedure SetDescription(const Value: string);
    procedure SetIsCompleted(const Value: Boolean);
    procedure SetIsReadOnly(const Value: Boolean);
    procedure SetMode(const Value: TMode);
  public
    procedure Add;
    procedure DescriptionDblClick(Sender: TObject);
    procedure DescriptionExit(Sender: TObject);
    procedure DescriptionKeyPress(Sender: TObject; var Key: Char);
    procedure DescriptionKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure Remove;
    property AddVisible: Boolean read GetAddVisible;
    property Description: string read FDescription write SetDescription;
    property IsCompleted: Boolean read FIsCompleted write SetIsCompleted;
    property IsReadOnly: Boolean read FIsReadOnly write SetIsReadOnly;
    property Mode: TMode read FMode write SetMode;
  end;

implementation

uses
  Controls;

procedure TTaskViewModel.Add;
var
  Task: ITaskViewModel;
begin
  if Length(Trim(Description)) > 0 then
  begin
    Task := TTaskViewModel.Create;
    (Task as TTaskViewModel).Mode := TMode.View;
    (Task as TTaskViewModel).Description := Description;
    IoC.Get<ITasksViewModel>.NewTask(Task);
    Description := '';
  end;
end;

procedure TTaskViewModel.DescriptionDblClick(Sender: TObject);
begin
  Mode := TMode.Edit;
end;

procedure TTaskViewModel.DescriptionExit(Sender: TObject);
begin
  case Mode of
    TMode.Edit:
      Mode := TMode.View;
  end;
end;

procedure TTaskViewModel.DescriptionKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    Key := 0;
    case Mode of
      TMode.New:
        Add;
      TMode.View:
        Mode := TMode.Edit;
      TMode.Edit:
        Mode := TMode.View;
    end;
  end;
end;

procedure TTaskViewModel.DescriptionKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    case Mode of
      TMode.New:
        Add;
      TMode.View:
        Mode := TMode.Edit;
      TMode.Edit:
        Mode := TMode.View;
    end;
  end;
end;

function TTaskViewModel.GetAddVisible: Boolean;
begin
  Result := Length(Trim(Description)) > 0;
end;

procedure TTaskViewModel.Remove;
begin
  IoC.Get<ITasksViewModel>.RemoveTask(Self);
end;

procedure TTaskViewModel.SetDescription(const Value: string);
begin
  FDescription := Value;
  NotifyOfPropertyChange('Description');
  NotifyOfPropertyChange('AddVisible');
end;

procedure TTaskViewModel.SetIsCompleted(const Value: Boolean);
begin
  FIsCompleted := Value;
  NotifyOfPropertyChange('IsCompleted');
end;

procedure TTaskViewModel.SetIsReadOnly(const Value: Boolean);
begin
  FIsReadOnly := Value;
  NotifyOfPropertyChange('IsReadOnly');
end;

procedure TTaskViewModel.SetMode(const Value: TMode);
begin
  FMode := Value;
  NotifyOfPropertyChange('Mode');
end;

initialization

TTaskViewModel.ClassName;

end.
