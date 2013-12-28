unit AppViewModel;

interface

uses
  AppInterfaces,
  DSharp.PresentationModel;

type
  TAppViewModel = class(TScreen, IAppViewModel)
  strict private
    FCount: Integer;
    FIncrementValue: Integer;
  strict protected
    function GetCanDecrementCount(): Boolean; virtual;
    function GetCanIncrementCount(): Boolean; virtual;
    function GetCanIncrementCountBy2(): Boolean; virtual;
    function GetCanIncrementCountByIncrementValue(): Boolean; virtual;
    function GetCanMultiplyCountBy2(): Boolean; virtual;
    function GetCount(): Integer; virtual;
    function GetIncrementValue(): Integer; virtual;
    procedure SetCount(const Value: Integer); virtual;
    procedure SetIncrementValue(const Value: Integer); virtual;
  public
    constructor Create(); override;
    procedure DecrementCount(); virtual;
    procedure IncrementCount(); virtual;
    procedure IncrementCountBy2(); virtual;
    procedure IncrementCountByIncrementValue(); virtual;
    procedure MultiplyCountBy2(); virtual;
    property CanDecrementCount: Boolean read GetCanDecrementCount;
    property CanIncrementCount: Boolean read GetCanIncrementCount;
    property CanIncrementCountBy2: Boolean read GetCanIncrementCountBy2;
    property CanIncrementCountByIncrementValue: Boolean
      read GetCanIncrementCountByIncrementValue;
    property CanMultiplyCountBy2: Boolean read GetCanMultiplyCountBy2;
    property Count: Integer read GetCount write SetCount;
    property IncrementValue: Integer read GetIncrementValue
      write SetIncrementValue;
  end;

implementation

uses
  Classes,
  Spring;

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

function TAppViewModel.GetCanIncrementCountBy2(): Boolean;
begin
  Result := Count + 1 < MaximumCount;
end;

function TAppViewModel.GetCanIncrementCountByIncrementValue(): Boolean;
begin
  Result := (Count + IncrementValue >= MinimumCount) and
    (Count + IncrementValue <= MaximumCount);
end;

function TAppViewModel.GetCanMultiplyCountBy2(): Boolean;
begin
  Result := (Count * 2 <= MaximumCount) and (Count * 2 >= MinimumCount);
end;

function TAppViewModel.GetCount(): Integer;
begin
  Result := FCount;
end;

function TAppViewModel.GetIncrementValue(): Integer;
begin
  Result := FIncrementValue;
end;

procedure TAppViewModel.IncrementCount;
begin
  if not CanIncrementCount then
    raise EInvalidOperation.Create('not CanIncrementCount');
  Count := Count + 1;
end;

procedure TAppViewModel.IncrementCountBy2();
begin
  if not CanIncrementCountBy2 then
    raise EInvalidOperation.Create('not CanIncrementCountBy2');
  Count := Count + 2;
end;

procedure TAppViewModel.IncrementCountByIncrementValue();
begin
  if not CanIncrementCountByIncrementValue then
    raise EInvalidOperation.Create('not CanIncrementCountByIncrementValue');
  Count := Count + IncrementValue;
end;

procedure TAppViewModel.MultiplyCountBy2();
begin
  if not CanMultiplyCountBy2 then
    raise EInvalidOperation.Create('not CanMultiplyCountBy2');
  Count := Count * 2;
end;

procedure TAppViewModel.SetCount(const Value: Integer);
begin
  Guard.CheckRange((Value >= MinimumCount) and (Value <= MaximumCount), 'Value');
  if Count <> Value then
  begin
    FCount := Value;
    NotifyOfPropertyChange('Count');
    NotifyOfPropertyChange('CanDecrementCount');
    NotifyOfPropertyChange('CanIncrementCount');
    NotifyOfPropertyChange('CanIncrementCountBy2');
    NotifyOfPropertyChange('CanIncrementCountByIncrementValue');
    NotifyOfPropertyChange('CanMultiplyCountBy2');
  end;
end;

procedure TAppViewModel.SetIncrementValue(const Value: Integer);
begin
  if IncrementValue <> Value then
  begin
    FIncrementValue := Value;
    NotifyOfPropertyChange('IncrementValue');
    NotifyOfPropertyChange('CanIncrementCountByIncrementValue');
  end;
end;

initialization
  TAppViewModel.ClassName;
end.
