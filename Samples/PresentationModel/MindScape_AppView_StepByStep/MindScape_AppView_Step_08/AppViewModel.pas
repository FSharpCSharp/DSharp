unit AppViewModel;

interface

uses
  AppInterfaces,
  DSharp.PresentationModel;

type
  TAppViewModel = class(TScreen, IAppViewModel)
  strict private
    FCount: Integer;
  strict protected
    function GetCanDecrementCount(): Boolean; virtual;
    function GetCanIncrementCount(): Boolean; virtual;
    function GetCount(): Integer; virtual;
    procedure SetCount(const Value: Integer); virtual;
  public
    constructor Create(); override;
    procedure DecrementCount(); virtual;
    procedure IncrementCount(); virtual;
    property CanDecrementCount: Boolean read GetCanDecrementCount;
    property CanIncrementCount: Boolean read GetCanIncrementCount;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

uses
  Classes;

constructor TAppViewModel.Create();
begin
  inherited Create();
  DisplayName := IAppViewModel_DisplayName;
end;

function TAppViewModel.GetCanDecrementCount(): Boolean;
begin
  Result := Count > MinimumCount;
end;

function TAppViewModel.GetCanIncrementCount(): Boolean;
begin
  Result := Count < MaximumCount;
end;

procedure TAppViewModel.DecrementCount;
begin
  if not CanDecrementCount then
    raise EInvalidOperation.Create('not CanDecrementCount');
  Count := Count - 1;
end;

function TAppViewModel.GetCount(): Integer;
begin
  Result := FCount;
end;

procedure TAppViewModel.IncrementCount;
begin
  if not CanIncrementCount then
    raise EInvalidOperation.Create('not CanIncrementCount');
  Count := Count + 1;
end;

procedure TAppViewModel.SetCount(const Value: Integer);
begin
  if Count <> Value then
  begin
    FCount := Value;
    NotifyOfPropertyChange('Count');
    NotifyOfPropertyChange('CanDecrementCount');
    NotifyOfPropertyChange('CanIncrementCount');
  end;
end;

initialization
  TAppViewModel.ClassName;
end.
