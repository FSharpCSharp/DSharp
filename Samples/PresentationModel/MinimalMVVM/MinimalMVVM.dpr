program MinimalMVVM;

uses
  {$IFDEF CodeSite}
  DSharp.Logging.CodeSite,
  {$ENDIF CodeSite}
  DSharp.PresentationModel.VCLApplication,
  Forms,
  ConverterViewForm in 'ConverterViewForm.pas' {ConverterView} ,
  ConverterViewModel in 'ConverterViewModel.pas',
  ConverterInterfaces in 'ConverterInterfaces.pas',
  TextConverter in 'TextConverter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  {$IFDEF CodeSite}
  Application.WithLogger<TCodeSiteLog>;
  {$ENDIF CodeSite}
  Application.Start<IConverterViewModel>();

end.
