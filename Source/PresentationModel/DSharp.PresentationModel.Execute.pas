unit DSharp.PresentationModel.Execute;

interface

uses
  Classes,
  SysUtils;

type
  ///	<summary>
  ///	  Enables easy marshalling of code to the UI / background thread.
  ///	</summary>
  Execute = class
  private
  class var
    FBackgroundThreadExecutor: TProc<TProc>;
    FUIThreadExecutor: TProc<TProc>;
  public
    class constructor Create;

    ///	<summary>
    ///	  Initializes the framework using the current dispatcher.
    ///	</summary>
    class procedure InitializeWithDispatcher; static;

    ///	<summary>
    ///	  Executes the action on the background thread.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure OnBackgroundThread(Action: TProc); static;

    ///	<summary>
    ///	  Executes the action on the UI thread.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure OnUIThread(Action: TProc); static;

    ///	<summary>
    ///	  Executes the action on the UI thread. But it makes sure this is
    ///	  called from the background thread.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure QueueActionOnUIThread(Action: TProc); static;

    ///	<summary>
    ///	  Resets the executor to use a non-dispatcher-based action executor.
    ///	</summary>
    class procedure ResetWithoutDispatcher; static;

    ///	<summary>
    ///	  Sets a custom background thread marshaller.
    ///	</summary>
    ///	<param name="Marshaller">
    ///	  The marshaller.
    ///	</param>
    class procedure SetBackgroundThreadMarshaller
      (Marshaller: TProc<TProc>); static;

    ///	<summary>
    ///	  Sets a custom UI thread marshaller.
    ///	</summary>
    ///	<param name="Marshaller">
    ///	  The marshaller.
    ///	</param>
    class procedure SetUIThreadMarshaller(Marshaller: TProc<TProc>); static;
  end;

implementation

uses
  Windows;

{ Execute }

class constructor Execute.Create;
begin
  FUIThreadExecutor := procedure(Action: TProc)
    begin
      Action();
    end;
  FBackgroundThreadExecutor := procedure(Action: TProc)
    begin
      Action();
    end;
end;

class procedure Execute.InitializeWithDispatcher;
begin
  SetUIThreadMarshaller(
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
    end);

  SetBackgroundThreadMarshaller(
    procedure(Action: TProc)
    begin
      TThread.CreateAnonymousThread(
        procedure
        begin
          Action();
        end).Start;
    end);
end;

class procedure Execute.OnBackgroundThread(Action: TProc);
begin
  FBackgroundThreadExecutor(Action);
end;

class procedure Execute.OnUIThread(Action: TProc);
begin
  FUIThreadExecutor(Action);
end;

class procedure Execute.QueueActionOnUIThread(Action: TProc);
begin
  FBackgroundThreadExecutor(
    procedure
    begin
      FUIThreadExecutor(Action);
    end);
end;

class procedure Execute.ResetWithoutDispatcher;
begin
  SetUIThreadMarshaller(
    procedure(Action: TProc)
    begin
      Action();
    end);

  SetBackgroundThreadMarshaller(
    procedure(Action: TProc)
    begin
      Action();
    end);
end;

class procedure Execute.SetBackgroundThreadMarshaller(Marshaller: TProc<TProc>);
begin
  FBackgroundThreadExecutor := Marshaller;
end;

class procedure Execute.SetUIThreadMarshaller(Marshaller: TProc<TProc>);
begin
  FUIThreadExecutor := Marshaller;
end;

end.
