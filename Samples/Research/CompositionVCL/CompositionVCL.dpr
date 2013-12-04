program CompositionVCL;

uses
  Vcl.Forms,
  zFormMain in 'zFormMain.pas' {FormMain} ,
  zFrameFirst in 'zFrameFirst.pas' {FrameFirst: TFrame} ,
  zFrameSecond in 'zFrameSecond.pas' {FrameSecond: TFrame} ,
  zShared in 'zShared.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
