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
    function GetCount(): Integer;
    procedure IncrementCount();
    procedure SetCount(const Value: Integer);
    property Count: Integer read GetCount write SetCount;
  end;

implementation

end.
