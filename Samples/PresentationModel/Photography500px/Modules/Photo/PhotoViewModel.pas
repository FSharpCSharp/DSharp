unit PhotoViewModel;

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Interfaces,
  DSharp.Collections,
  DSharp.ComponentModel.Composition,
  DSharp.PresentationModel,
  DSharp.PresentationModel.ConductorWithCollectionOneActive,
  Vcl.Imaging.jpeg;

type
  ///	<summary>
  ///	  Implementation of <see cref="IPhotoViewModel" />
  ///	</summary>
  // [PartCreationPolicy(cpShared)]
  TPhotoViewModel = class(TScreen, IPhotoViewModel)
  private
    { Private declarations }
    FPhoto: TPicture;
    procedure LoadRandomPicture;
  public
    { Public declarations }
    constructor Create; override;
    destructor Destroy; override;
    property Photo: TPicture read FPhoto;
  end;

implementation

uses
  IOUtils,
  ExtCtrls;

{ TPhotoViewModel }

constructor TPhotoViewModel.Create;
begin
  inherited;
  FPhoto := TPicture.Create;
  ViewAttached.Add(
    procedure(Sender: TObject; EventArgs: IViewAttachedEventArgs)
    begin
      LoadRandomPicture;
    end);
end;

destructor TPhotoViewModel.Destroy;
begin
  FPhoto.Free;
  inherited;
end;

procedure TPhotoViewModel.LoadRandomPicture;
var
  LFilename: string;
begin
  Execute.OnBackgroundThread(
    procedure
    begin
      Sleep(100 + Random(300));
      LFilename := Format('Images\%d.jpg', [1 + Random(19)]);
      if TFile.Exists(LFilename) then
      begin
        Execute.OnUIThread(
          procedure
          begin
            FPhoto.LoadFromFile(LFilename);
            TImage(TComponent(GetView).FindComponent('Photo')).Picture
              := FPhoto;
          end);
      end;
    end);
end;

initialization

TPhotoViewModel.ClassName;

end.
