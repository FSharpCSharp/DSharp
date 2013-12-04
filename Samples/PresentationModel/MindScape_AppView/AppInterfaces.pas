unit AppInterfaces;

interface

uses
  DSharp.PresentationModel;

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
    property IncrementValue: Integer read GetIncrementValue
      write SetIncrementValue;
  end;

  [InheritedExport]
  IAppViewModel = interface
    ['{38C5B8ED-8269-463D-847D-09F6A7B99584}']
  end;

implementation

end.
