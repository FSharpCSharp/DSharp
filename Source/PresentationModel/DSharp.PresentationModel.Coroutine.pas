unit DSharp.PresentationModel.Coroutine;

interface

uses
  SysUtils,
  DSharp.Core.EventArgs,
  DSharp.Collections,
  DSharp.Logging,
  DSharp.PresentationModel.ActionExecutionContext,
  DSharp.PresentationModel.EventAggregator,
  DSharp.PresentationModel.ResultIntf,
  DSharp.PresentationModel.ResultCompletionEventArgsIntf;

type
  ///	<summary>
  ///	  Manages coroutine execution.
  ///	</summary>
  TCoroutine = class
  private
  class var
    FCreateParentEnumerator: TFunc<IEnumerator<IResult>, IResult>;
    FLog: ILog;
    FResultCompletion: TResultCompletionEvent;

    class function GetLog: ILog; static;
    class property Log: ILog read GetLog;
  public
    class constructor Create;

    ///	<summary>
    ///	  Executes a coroutine.
    ///	</summary>
    ///	<param name="Coroutine">
    ///	  The coroutine to execute.
    ///	</param>
    ///	<param name="Context">
    ///	  The context to execute the coroutine within.
    ///	</param>
    ///	<param name="Callback">
    ///	  The completion callback for the coroutine.
    ///	</param>
    class procedure BeginExecute(Coroutine: IEnumerator<IResult>;
      Context: IActionExecutionContext = nil); overload; static;
    class procedure BeginExecute(Coroutine: IEnumerator<IResult>;
      Context: IActionExecutionContext; Callback: TResultCompletionEvent);
      overload; static;

    ///	<summary>
    ///	  Called upon completion of a TCoroutine.
    ///	</summary>
    class procedure Completed(Sender: TObject;
      Args: IResultCompletionEventArgs); static;

    ///	<summary>
    ///	  Creates the parent enumerator.
    ///	</summary>
    class property CreateParentEnumerator: TFunc<IEnumerator<IResult>, IResult>
      read FCreateParentEnumerator write FCreateParentEnumerator;
  end;

implementation

uses
  DSharp.PresentationModel.IoC,
  DSharp.PresentationModel.SequentialResult;

{ TCoroutine }

class constructor TCoroutine.Create;
begin
  FCreateParentEnumerator := function(Inner: IEnumerator<IResult>): IResult
    begin
      Result := TSequentialResult.Create(Inner);
    end;

  FResultCompletion := Completed;
end;

class function TCoroutine.GetLog: ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(TCoroutine));
  end;
  Result := FLog;
end;

class procedure TCoroutine.BeginExecute(Coroutine: IEnumerator<IResult>;
  Context: IActionExecutionContext = nil);
begin
  BeginExecute(Coroutine, Context, nil);
end;

class procedure TCoroutine.BeginExecute(Coroutine: IEnumerator<IResult>;
  Context: IActionExecutionContext; Callback: TResultCompletionEvent);
var
  LEnumerator: IResult;
begin
  Log.LogMessage('Executing coroutine.');

  LEnumerator := CreateParentEnumerator(Coroutine);
  IoC.BuildUp(LEnumerator as TObject);

  if Assigned(Callback) then
  begin
    LEnumerator.Completed.Add(Callback);
  end;

  LEnumerator.Completed.Add(FResultCompletion);

  if not Assigned(Context) then
  begin
    Context := TActionExecutionContext.Create;
  end;

  LEnumerator.Execute(Context);
end;

class procedure TCoroutine.Completed(Sender: TObject;
  Args: IResultCompletionEventArgs);
var
  LEnumerator: IResult;
begin
  if Supports(Sender, IResult, LEnumerator) then
  begin
    LEnumerator.Completed.Remove(FResultCompletion);

    if Assigned(Args.Error) then
    begin
      Log.LogException(Args.Error);
    end
    else if Args.WasCancelled then
    begin
      Log.LogMessage('Coroutine execution cancelled.');
    end
    else
    begin
      Log.LogMessage('Coroutine execution completed.');
    end;
  end;
end;

end.
