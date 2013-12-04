unit zShellViewModel;

interface

uses
  SysUtils,
  Interfaces,
  DSharp.PresentationModel;

type
  TShellViewModel = class(TScreen, IShellViewModel)
  private
    FName: string;
    function GetCanSayHello: Boolean;
    procedure SetName(const Value: string); reintroduce;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SayHello;
    property Name: string read FName write SetName;
    property CanSayHello: Boolean read GetCanSayHello;
  end;

implementation

uses
  Dialogs,
  HelloLogger,
  DSharp.Logging;

constructor TShellViewModel.Create;
begin
  inherited;
  THelloLogger.Log.LogMessage('TShellViewModel.Create()');
  ShowMessage('Create!');
end;

destructor TShellViewModel.Destroy;
var
  Log: ILog;
begin
  Log := THelloLogger.Log;
  if Assigned(Log) then
    THelloLogger.Log.LogMessage('TShellViewModel.Destroy()');
  ShowMessage('Destroy!');
  inherited;
end;

function TShellViewModel.GetCanSayHello: Boolean;
begin
  Result := Trim(FName) <> '';
end;

procedure TShellViewModel.SayHello;
begin
  THelloLogger.Log.LogMessage('TShellViewModel.SayHello(Name=%s)', [Name]);
  ShowMessage(Format('Hello %s!', [Name]));
end;

procedure TShellViewModel.SetName(const Value: string);
begin
  THelloLogger.Log.LogMessage('TShellViewModel.SetName(%s)', [Name]);
  FName := Value;
  NotifyOfPropertyChange('Name');
  NotifyOfPropertyChange('CanSayHello');
end;

initialization

TShellViewModel.ClassName;

end.
