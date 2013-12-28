program MindScape_AppViewVCL_Step00;

uses
  Forms,
  AppViewForm in 'AppViewForm.pas' {AppView};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize();
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAppView, AppView);
  Application.Run();
end.
