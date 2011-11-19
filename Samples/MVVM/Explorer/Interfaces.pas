unit Interfaces;

interface

uses
  DSharp.Collections,
  DSharp.ComponentModel.Composition;

type
  [InheritedExport]
  IMainViewModel = interface
    ['{0F41E083-A86A-4EAE-B9BB-6889D17B87CE}']
  end;

  [InheritedExport]
  IWorkingAreaViewModel = interface
    ['{060A761D-21FD-473E-9B9A-91F62FCE4467}']
  end;

  [InheritedExport]
  INavigationViewModel = interface
    ['{E95E8143-E718-4603-B658-DDF2DAD757D5}']
    function GetElements: IList<IWorkingAreaViewModel>;
    function GetSelectedElement: IWorkingAreaViewModel;
    procedure SetElements(const Value: IList<IWorkingAreaViewModel>);
    property SelectedElement: IWorkingAreaViewModel read GetSelectedElement;
    property Elements: IList<IWorkingAreaViewModel> read GetElements write SetElements;
  end;

implementation

end.
