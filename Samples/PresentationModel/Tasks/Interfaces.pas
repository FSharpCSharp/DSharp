unit Interfaces;

interface

uses
  DSharp.PresentationModel;

type

  ///	<summary>
  ///	  Interface IShellViewModel
  ///	</summary>
  [InheritedExport]
  IShellViewModel = interface
    ['{2414B671-F701-490A-A7A4-5A14EB8D37EA}']
  end;

  ///	<summary>
  ///	  Interface ITaskViewModel
  ///	</summary>
  [InheritedExport]
  ITaskViewModel = interface
    ['{71B3668B-A70F-4C58-9710-A14C5BBC675C}']
  end;

  ///	<summary>
  ///	  Interface ITasksViewModel
  ///	</summary>
  [InheritedExport]
  ITasksViewModel = interface
    ['{52106363-5231-4037-ADDB-7E52197F3C78}']
    procedure RemoveTask(Task: ITaskViewModel);
    procedure NewTask(Task: ITaskViewModel);
  end;

implementation

end.
