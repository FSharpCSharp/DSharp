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
  jpeg;

type
  /// <summary>
  /// Implementation of <see cref="IPhotoViewModel" />
  /// </summary>
  // [PartCreationPolicy(cpShared)]
  TPhotoViewModel = class(TScreen, IPhotoViewModel)
  private
    { Private declarations }
    FPicture: TPicture;
    procedure LoadRandomPicture;
  public
    { Public declarations }
    constructor Create; override;
    destructor Destroy; override;
    property Picture: TPicture read FPicture;
  end;

implementation

uses
  IOUtils,
  ExtCtrls;

{ TPhotoViewModel }

constructor TPhotoViewModel.Create;
begin
  inherited;
  FPicture := TPicture.Create;
  ViewAttached.Add(
    procedure(Sender: TObject; EventArgs: IViewAttachedEventArgs)
    begin
      LoadRandomPicture;
    end);
end;

destructor TPhotoViewModel.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TPhotoViewModel.LoadRandomPicture;
var
  LFilename: string;
begin
  Execute.OnBackgroundThread(
    procedure
    begin
//      Sleep(Random(500));
      LFilename := Format('Images\%d.jpg', [1 + Random(19)]);
      FPicture.LoadFromFile(LFilename);
      NotifyOfPropertyChange('Picture');
    end);
end;

initialization

TPhotoViewModel.ClassName;

end.
