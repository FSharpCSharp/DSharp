program Photography500px;

uses
  DSharp.PresentationModel.VCLApplication,
  Vcl.Forms,
  ShellViewForm in 'Modules\Shell\ShellViewForm.pas' {ShellView} ,
  ShellViewModel in 'Modules\Shell\ShellViewModel.pas',
  Interfaces in 'Framework\Interfaces.pas',
  ApplicationBootstrapper in 'Framework\ApplicationBootstrapper.pas',
  GalleryViewFrame
    in 'Modules\Gallery\GalleryViewFrame.pas' {GalleryView: TFrame} ,
  GalleryViewModel in 'Modules\Gallery\GalleryViewModel.pas',
  PhotoViewFrame in 'Modules\Photo\PhotoViewFrame.pas' {PhotoView: TFrame} ,
  PhotoViewModel in 'Modules\Photo\PhotoViewModel.pas',
  Vcl.Themes,
  Vcl.Styles,
  AsyncCalls in 'Libs\AsyncCalls\AsyncCalls.pas',
  dwsJSON in 'Libs\dwsJSON\dwsJSON.pas',
  dwsUtils in 'Libs\dwsJSON\dwsUtils.pas',
  dwsXPlatform in 'Libs\dwsJSON\dwsXPlatform.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Start(TApplicationBootstrapper);

end.
