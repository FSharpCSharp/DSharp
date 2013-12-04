unit ApplicationBootstrapper;

interface

uses
  Classes,
  Sysutils,
  Interfaces,
  DSharp.PresentationModel,
  DSharp.PresentationModel.Bootstrapper;

type
  TApplicationBootstrapper = class(TSpringBootstrapper<IShellViewModel>)
  protected
    procedure Configure; override;
  end;

implementation

uses
  AsyncCalls;

{ TApplicationBootstrapper }

procedure TApplicationBootstrapper.Configure;
begin
  inherited;

  // Configure marshaller to execute OnBackgroundThread actions by AsyncCalls library
  Execute.SetBackgroundThreadMarshaller(
    procedure(Action: TProc)
    begin
      TAsyncCalls.Invoke(Action).Forget;
    end);
end;

end.
