unit DSharp.PresentationModel.SequentialResult;

interface

uses
  SysUtils,
  DSharp.Collections,
  DSharp.Core.EventArgs,
  DSharp.PresentationModel.CoroutineExecutionContextIntf,
  DSharp.PresentationModel.ResultBase,
  DSharp.PresentationModel.ResultCompletionEventArgsIntf,
  DSharp.PresentationModel.ResultIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IResult" /> that enables sequential
  ///	  execution of multiple results.
  ///	</summary>
  TSequentialResult = class(TResultBase)
  strict private
    FContext: ICoroutineExecutionContext;
    FChildCompletedEvent: TResultCompletionEvent;
    procedure ChildCompleted(Sender: TObject; Args: IResultCompletionEventArgs);
    procedure OnComplete(Error: Exception; WasCancelled: Boolean);
  private
    FEnumerator: IEnumerator<IResult>;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="SequentialResult" />class
    ///	  .
    ///	</summary>
    ///	<param name="enumerator">
    ///	  The enumerator.
    ///	</param>
    constructor Create(enumerator: IEnumerator<IResult>);

    ///	<summary>
    ///	  Executes the result using the specified context.
    ///	</summary>
    ///	<param name="context">
    ///	  The context.
    ///	</param>
    procedure Execute(Context: ICoroutineExecutionContext); override;
  end;

implementation

uses
  DSharp.PresentationModel.IoC,
  DSharp.PresentationModel.ResultCompletionEventArgs;

constructor TSequentialResult.Create(enumerator: IEnumerator<IResult>);
begin
  FEnumerator := enumerator;
  FChildCompletedEvent := ChildCompleted;
end;

{ TSequentialResult }

procedure TSequentialResult.ChildCompleted(Sender: TObject;
  Args: IResultCompletionEventArgs);
var
  LPrevious, LNext: IResult;
  LMoveNextSucceeded: Boolean;
begin
  if Supports(Sender, IResult, LPrevious) then
    LPrevious.Completed.Remove(FChildCompletedEvent);

  if Assigned(Args.Error) or Args.WasCancelled then
  begin
    OnComplete(Args.Error, Args.WasCancelled);
    Exit;
  end;

  try
    LMoveNextSucceeded := FEnumerator.MoveNext();
  except
    on Args: Exception do
    begin
      OnComplete(Args, false);
      Exit;
    end;
  end;

  if LMoveNextSucceeded then
  begin
    try
      LNext := FEnumerator.Current;
      IoC.BuildUp(LNext as TObject);
      LNext.Completed.Add(FChildCompletedEvent);
      LNext.Execute(FContext);
    except
      on Args: Exception do
      begin
        OnComplete(Args, false);
        Exit;
      end;
    end;
  end
  else
  begin
    OnComplete(nil, false);
  end;
end;

procedure TSequentialResult.Execute(Context: ICoroutineExecutionContext);
var
  LArgs: IResultCompletionEventArgs;
begin
  FContext := Context;
  LArgs := TResultCompletionEventArgs.Create;
  ChildCompleted(nil, LArgs);
end;

procedure TSequentialResult.OnComplete(Error: Exception; WasCancelled: Boolean);
var
  LArgs: IResultCompletionEventArgs;
begin
  FEnumerator := nil;
  LArgs := TResultCompletionEventArgs.Create(Error, WasCancelled);
  Completed.Invoke(Self, LArgs);
end;

end.
