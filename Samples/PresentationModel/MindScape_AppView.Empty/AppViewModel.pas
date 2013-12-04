unit AppViewModel;

interface

uses
  AppInterfaces,
  DSharp.PresentationModel;

type
  TAppViewModel = class(TScreen, IAppViewModel)
//  strict private
//    FAppModel: IAppModel;
//    FCount: IAppModel;
//A  strict protected
//A    function GetCanDecrementCount(): Boolean; virtual;
//A    function GetCanIncrementCount(): Boolean; virtual;
//B    function GetCanIncrementCountBy2(): Boolean; virtual;
//A    function GetCanIncrementCountByIncrementValue(): Boolean; virtual;
//C    function GetCanMultiplyCountBy2(): Boolean; virtual;
//A    function GetCount(): Integer; virtual;
//B    function GetIncrementValue(): Integer; virtual;
//A    procedure SetCount(const Value: Integer); virtual;
//A    procedure SetIncrementValue(const Value: Integer); virtual;
//C    property AppModel: IAppModel read FAppModel;
//A  public
//E    constructor Create(const AAppModel: IAppModel);
//A    procedure DecrementCount(); virtual;
//A    procedure IncrementCount(); virtual;
//B    procedure IncrementCountBy2(); virtual;
//B    procedure IncrementCountByIncrementValue(); virtual;
//B    procedure MultiplyCountBy2(); virtual;
//A    property CanDecrementCount: Boolean read GetCanDecrementCount;
//A    property CanIncrementCount: Boolean read GetCanIncrementCount;
//B    property CanIncrementCountBy2: Boolean read GetCanIncrementCountBy2;
//B    property CanIncrementCountByIncrementValue: Boolean read GetCanIncrementCountByIncrementValue;
//B    property CanMultiplyCountBy2: Boolean read GetCanMultiplyCountBy2;
//A    property Count: Integer read GetCount write SetCount;
//B    property IncrementValue: Integer read GetIncrementValue write SetIncrementValue;
  end;

implementation

//uses
//  SysUtils;
//
//constructor TAppViewModel.Create(const AAppModel: IAppModel);
//begin
//  inherited Create();
//  FAppModel := AAppModel;
//end;
//
//function TAppViewModel.GetCanDecrementCount(): Boolean;
//begin
//  Result := Count > MinimumCount;
//end;
//
//function TAppViewModel.GetCanIncrementCount(): Boolean;
//begin
//  Result := Count < MaximumCount;
//end;
//
//procedure TAppViewModel.DecrementCount;
//begin
//  Count := Count - 1;
//end;
//
//function TAppViewModel.GetCanIncrementCountBy2(): Boolean;
//begin
//  Result := Count + 1 < MaximumCount;
//end;
//
//function TAppViewModel.GetCanIncrementCountByIncrementValue(): Boolean;
//begin
//  Result := (Count + IncrementValue >= MinimumCount) and
//            (Count + IncrementValue <= MaximumCount);
//end;
//
//function TAppViewModel.GetCanMultiplyCountBy2(): Boolean;
//begin
//  Result := (Count * 2 <= MaximumCount) and (Count * 2 >= MinimumCount);
//end;
//
//function TAppViewModel.GetCount(): Integer;
//begin
//A  Result := FCount;
//C  Result := AppModel.Count;
//end;
//
//function TAppViewModel.GetIncrementValue(): Integer;
//begin
//  Result := AppModel.IncrementValue;
//end;
//
//procedure TAppViewModel.IncrementCount;
//begin
//  Count := Count + 1;
//end;
//
//procedure TAppViewModel.IncrementCountBy2();
//begin
//  Count := Count + 2;
//end;
//
//procedure TAppViewModel.IncrementCountByIncrementValue();
//begin
//  Count := Count + IncrementValue;
//end;
//
//procedure TAppViewModel.MultiplyCountBy2();
//begin
//  Count := Count * 2;
//end;
//
//procedure TAppViewModel.SetCount(const Value: Integer);
//begin
//  if Count <> Value then
//  begin
//A    FCount := Value;
//C    AppModel.Count := Value;
//A    // if Delphi adds proper lamda expressions (versus anonymous methods) http://en.wikipedia.org/wiki/Lambda_(programming)
//A    // then we can get rid of the strings and do something like this:
//A    // NotifyOfPropertyChange(() => Count);
//A    NotifyOfPropertyChange('Count');
//A    NotifyOfPropertyChange('CanDecrementCount');
//A    NotifyOfPropertyChange('CanIncrementCount');
//B    NotifyOfPropertyChange('CanIncrementCountBy2');
//C    NotifyOfPropertyChange('CanIncrementCountByIncrementValue');
//B    NotifyOfPropertyChange('CanMultiplyCountBy2');
//  end;
//end;

//B
//procedure TAppViewModel.SetIncrementValue(const Value: Integer);
//begin
//  if IncrementValue <> Value then
//  begin
//B    AppModel.IncrementValue := Value;
//C    AppModel.IncrementValue := Value;
//    NotifyOfPropertyChange('IncrementValue');
//    NotifyOfPropertyChange('CanIncrementCountByIncrementValue');
//  end;
//end;

initialization
  TAppViewModel.ClassName;
end.
