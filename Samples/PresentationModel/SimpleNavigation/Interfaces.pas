unit Interfaces;

interface

uses
  DSharp.PresentationModel;

type

  ///	<summary>
  ///	  Interface IPageOneViewModel
  ///	</summary>
  [InheritedExport]
  IPageOneViewModel = interface(IScreen)
    ['{CDA46508-E4C5-408B-A30F-D85258DD5CF1}']
  end;

  ///	<summary>
  ///	  Interface IPageTwoViewModel
  ///	</summary>
  [InheritedExport]
  IPageTwoViewModel = interface(IScreen)
    ['{C18A8F8E-9310-496A-B19F-65FD34B69580}']
  end;

  ///	<summary>
  ///	  Interface IShellViewModel
  ///	</summary>
  [InheritedExport]
  IShellViewModel = interface(IScreen)
    ['{24847D30-5F8F-4F8F-8788-D6A2A1A63B95}']
  end;

implementation

end.
