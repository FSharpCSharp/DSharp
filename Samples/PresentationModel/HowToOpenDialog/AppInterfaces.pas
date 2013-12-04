unit AppInterfaces;

interface

uses
  DSharp.PresentationModel;

type

  [InheritedExport]
  IAppViewModel = interface
    ['{1EBA696F-5611-48A1-99B4-914C789ED61C}']
  end;

  [InheritedExport]
  IMyDialogViewModel = interface
    ['{6956F2A0-E255-4B85-8B99-3AB40FC3F189}']
    function GetValue: string;
    procedure SetValue(const Value: string);
    property Value: string read GetValue write SetValue;
  end;

implementation

end.
