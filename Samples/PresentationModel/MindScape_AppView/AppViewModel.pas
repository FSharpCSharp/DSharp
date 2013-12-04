unit AppViewModel;

interface

uses
  AppInterfaces,
  DSharp.PresentationModel;

type
  TAppViewModel = class(TScreen, IAppViewModel)
  strict private
    FAppModel: IAppModel;
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
    property AppModel: IAppModel read FAppModel;
  public
    constructor Create(const AAppModel: IAppModel);
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
  SysUtils;

constructor TAppViewModel.Create(const AAppModel: IAppModel);
begin
  inherited Create();
  FAppModel := AAppModel;
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
  Result := AppModel.Count;
end;

function TAppViewModel.GetIncrementValue(): Integer;
begin
  Result := AppModel.IncrementValue;
end;

procedure TAppViewModel.IncrementCount;
begin
  Count := Count + 1;
end;

procedure TAppViewModel.IncrementCountBy2();
begin
  Count := Count + 2;
end;

procedure TAppViewModel.IncrementCountByIncrementValue();
begin
  Count := Count + IncrementValue;
end;

procedure TAppViewModel.MultiplyCountBy2();
begin
  Count := Count * 2;
end;

procedure TAppViewModel.SetCount(const Value: Integer);
begin
  if Count <> Value then
  begin
    AppModel.Count := Value;
    // if Delphi adds proper lamda expressions (versus anonymous methods) http://en.wikipedia.org/wiki/Lambda_(programming)
    // then we can get rid of the strings and do something like this:
    // NotifyOfPropertyChange(() => Count);
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
    AppModel.IncrementValue := Value;
    NotifyOfPropertyChange('IncrementValue');
    NotifyOfPropertyChange('CanIncrementCountByIncrementValue');
  end;
end;

initialization

TAppViewModel.ClassName;

end.
