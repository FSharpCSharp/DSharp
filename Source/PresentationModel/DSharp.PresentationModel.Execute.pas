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
    ///	  Initializes the framework with custom executors
    ///	</summary>
    class procedure Initialize(UIThreadExecutor: TProc<TProc>;
      BackgroundThreadExecutor: TProc<TProc>);

    ///	<summary>
    ///	  Executes the action on the UI thread without waiting for it to
    ///	  complete.
    ///	</summary>
    ///	<param name="Action">
    ///	  The action to execute.
    ///	</param>
    class procedure BeginOnUIThread(Action: TProc); static;

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
  end;

implementation

uses
  Windows;

{ Execute }

class constructor Execute.Create;
begin
  FUIThreadExecutor := procedure(Action: TProc)
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
    end;

  FBackgroundThreadExecutor := procedure(Action: TProc)
    begin
      TThread.CreateAnonymousThread(
        procedure
        begin
          Action();
        end).Start;
    end;
end;

class procedure Execute.Initialize(UIThreadExecutor, BackgroundThreadExecutor
  : TProc<TProc>);
begin
  FUIThreadExecutor := UIThreadExecutor;
  FBackgroundThreadExecutor := BackgroundThreadExecutor;
end;

class procedure Execute.BeginOnUIThread(Action: TProc);
begin
  FBackgroundThreadExecutor(
    procedure
    begin
      FUIThreadExecutor(Action);
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

end.
