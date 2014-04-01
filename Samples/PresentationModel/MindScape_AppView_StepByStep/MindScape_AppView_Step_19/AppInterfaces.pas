unit AppInterfaces;

interface

uses
  DSharp.PresentationModel;

resourcestring
  IAppViewModel_DisplayName = 'Increments...';

const
  MinimumCount = -10;
  MaximumCount = +10;

type
  [InheritedExport]
  IAppModel = interface
    ['{DD3AABF1-140F-4F78-85E3-2E332218F8AE}']
    function GetCount(): Integer;
    function GetIncrementValue(): Integer;
    procedure SetCount(const Value: Integer);
    procedure SetIncrementValue(const Value: Integer);
    property Count: Integer read GetCount write SetCount;
    property IncrementValue: Integer read GetIncrementValue write SetIncrementValue;
  end;

  [InheritedExport]
  IAppViewModel = interface
    ['{38C5B8ED-8269-463D-847D-09F6A7B99584}']
    procedure DecrementCount();
    function GetCanIncrementCountBy2(): Boolean;
    function GetCanIncrementCountByIncrementValue(): Boolean;
    function GetCanMultiplyCountBy2(): Boolean;
    function GetCount(): Integer;
    function GetIncrementValue(): Integer;
    procedure IncrementCount();
    procedure IncrementCountBy2();
    procedure IncrementCountByIncrementValue();
    procedure MultiplyCountBy2();
    procedure SetCount(const Value: Integer);
    procedure SetIncrementValue(const Value: Integer);
    property CanIncrementCountBy2: Boolean read GetCanIncrementCountBy2;
    property CanIncrementCountByIncrementValue: Boolean read GetCanIncrementCountByIncrementValue;
    property CanMultiplyCountBy2: Boolean read GetCanMultiplyCountBy2;
    property Count: Integer read GetCount write SetCount;
    property IncrementValue: Integer read GetIncrementValue write SetIncrementValue;
  end;

implementation

end.
