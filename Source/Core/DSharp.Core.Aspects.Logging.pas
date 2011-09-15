unit DSharp.Core.Aspects.Logging;

interface

uses
  DSharp.Core.Aspects,
  DSharp.Core.Logging,
  Rtti,
  SysUtils;

type
  TLoggingAspect = class(TAspect)
  protected
    class procedure DoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue); override;
    class procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean;
      out Result: TValue); override;
    class procedure DoException(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out RaiseException: Boolean;
      Exception: Exception; out Result: TValue); override;
  end;

implementation

{ TLoggingAspect }

class procedure TLoggingAspect.DoAfter(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; var Result: TValue);
begin
  if Method.ReturnType <> nil then
    Logging.LogValue('Result', Result);
  Logging.LeaveMethod(Instance, Method.Name);
end;

class procedure TLoggingAspect.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
var
  i: Integer;
  LParams: TArray<TRttiParameter>;
begin
  Logging.EnterMethod(Instance, Method.Name);
  LParams := Method.GetParameters();
  for i := Low(Args) to High(Args) do
    Logging.LogValue(LParams[i].Name, Args[i]);
end;

class procedure TLoggingAspect.DoException(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out RaiseException: Boolean; Exception: Exception;
  out Result: TValue);
begin
  Logging.LogException(Exception);
end;

end.
