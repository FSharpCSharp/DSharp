unit Interfaces;

interface

uses
  Classes,
  DSharp.Core.Events,
  DSharp.PresentationModel;

type

  ///	<summary>
  ///	  Interface IShellViewModel
  ///	</summary>
  [InheritedExport]
  IShellViewModel = interface(IScreen)
    ['{7B34DEAF-5052-4E15-92D0-81909A734E07}']
  end;

  ///	<summary>
  ///	  Interface IExampleOneViewModel
  ///	</summary>
  [InheritedExport]
  IExampleOneViewModel = interface(IScreen)
    ['{A0DD952D-5720-4FAB-AB8C-4E875EE158C4}']
  end;

  ///	<summary>
  ///	  Interface IExampleTwoViewModel
  ///	</summary>
  [InheritedExport]
  IExampleTwoViewModel = interface(IScreen)
    ['{650F483B-8783-4636-AA6D-5868F2E32226}']
  end;

  ///	<summary>
  ///	  Interface IInterestViewModel
  ///	</summary>
  [InheritedExport]
  IInterestItemViewModel = interface(IScreen)
    ['{39B79002-78F5-4EA6-82AB-5E7A60ECEE13}']
    function GetIsSelected: Boolean;
    property IsSelected: Boolean read GetIsSelected;
  end;

  ///	<summary>
  ///	  Interface IInterestSelectorViewModel
  ///	</summary>
  [InheritedExport]
  IInterestSelectorViewModel = interface(IScreen)
    ['{4D4959ED-5591-4E81-A2EA-1FB88587756F}']
    function GetSelectedInterestsChanged: IEvent<TNotifyEvent>;
    procedure OnInterestSelectionChanged;
    property SelectedInterestsChanged: IEvent<TNotifyEvent>
      read GetSelectedInterestsChanged;
  end;

implementation

end.
