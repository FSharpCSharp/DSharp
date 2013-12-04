unit HelloLogger;

interface

uses
  DSharp.Logging;

type
  THelloLogger = class(TObject)
  strict private
    class var FEverCreatedLog: Boolean;
    class var FLog: ILog;
    class function GetLog(): ILog; static;
  public
    class property Log: ILog read GetLog;
  end;

implementation

uses
  SysUtils;

class function THelloLogger.GetLog(): ILog;
begin
  if not Assigned(FLog) then
  begin
    if FEverCreatedLog then
      // raise Exception.Create('Already created this logger once. Did the unit already finalize and clear the interface reference?')
    else
      FLog := LogManager.GetLog(TypeInfo(THelloLogger));
    FEverCreatedLog := True;
  end;
  Result := FLog;
end;

end.
