unit Interfaces;

interface

uses
  DSharp.ComponentModel.Composition;

type
  [InheritedExport]
  IMainViewModel = interface
    ['{0F41E083-A86A-4EAE-B9BB-6889D17B87CE}']
  end;

  [InheritedExport]
  INavigationViewModel = interface
    ['{E95E8143-E718-4603-B658-DDF2DAD757D5}']
  end;

  [InheritedExport]
  IWorkingAreaViewModel = interface
    ['{060A761D-21FD-473E-9B9A-91F62FCE4467}']
  end;

implementation

end.
