unit ApplicationBootstrapper;

interface

uses
  Classes,
  Sysutils,
  Interfaces,
  jpeg,
  DSharp.PresentationModel,
  DSharp.PresentationModel.SpringBootstrapper;

type
  TApplicationBootstrapper = class(TSpringBootstrapper<IShellViewModel>)
  protected
    procedure Configure; override;
  end;

implementation

uses
  AsyncCalls,
  Windows;

{ TApplicationBootstrapper }

procedure TApplicationBootstrapper.Configure;
begin
  inherited;

  // Configure executor to use AsyncCalls library
  Execute.Initialize(
    procedure(Action: TProc)
    begin
      if GetCurrentThreadId() = MainThreadID then
      begin
        Action();
      end
      else
      begin
        TThread.Queue(nil,
          procedure
          begin
            Action();
          end);
      end;
    end,
    procedure(Action: TProc)
    begin
      TAsyncCalls.Invoke(Action).Forget;
    end);
end;

end.
