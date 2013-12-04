unit Interfaces;

interface

uses
  Classes,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel;

type

  ///	<summary>
  ///	  Interface IShellViewModel
  ///	</summary>
  [InheritedExport]
  IShellViewModel = interface(IScreen)
    ['{F055E5B1-6BAF-4858-9D03-83BD9C04D19E}']
  end;

  ///	<summary>
  ///	  Interface IGalleryViewModel
  ///	</summary>
  [InheritedExport]
  IGalleryViewModel = interface(IScreen)
    ['{B57FFBBB-DA42-457F-A259-AC8EAA4C6BC2}']
  end;

  ///	<summary>
  ///	  Interface IPhotoViewModel
  ///	</summary>
  [InheritedExport]
  IPhotoViewModel = interface(IScreen)
    ['{B2D0CE00-B8AE-4FB1-ACD1-36C04217401D}']
  end;

implementation

end.
