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
  IAppViewModel = interface
    ['{38C5B8ED-8269-463D-847D-09F6A7B99584}']
    procedure DecrementCount();
    function GetCanIncrementCountBy2(): Boolean;
    function GetCanMultiplyCountBy2(): Boolean;
    function GetCount(): Integer;
    procedure IncrementCount();
    procedure IncrementCountBy2();
    procedure MultiplyCountBy2();
    procedure SetCount(const Value: Integer);
    property CanIncrementCountBy2: Boolean read GetCanIncrementCountBy2;
    property CanMultiplyCountBy2: Boolean read GetCanMultiplyCountBy2;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

end.
