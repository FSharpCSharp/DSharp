unit GalleryViewModel;

interface

uses
  Classes,
  SysUtils,
  Interfaces,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive,
  DSharp.PresentationModel.Inpc;

type
  ///	<summary>
  ///	  Implementation of <see cref="IGalleryViewModel" />
  ///	</summary>
  // [PartCreationPolicy(cpShared)]
  TGalleryViewModel = class(TScreen, IGalleryViewModel)
  private
    { Private declarations }
    FPhotoCollection: IList<IPhotoViewModel>;
  public
    { Public declarations }
    constructor Create; override;
    property PhotoCollection: IList<IPhotoViewModel> read FPhotoCollection;
  end;

implementation

uses
  PhotoViewModel;

{ TGalleryViewModel }

constructor TGalleryViewModel.Create;
var
  i: Integer;
begin
  inherited Create;
  FPhotoCollection := TBindableCollection<IPhotoViewModel>.Create;
  for i := 1 to 300 do
    FPhotoCollection.Add(TPhotoViewModel.Create);
end;

initialization

TGalleryViewModel.ClassName;

end.
