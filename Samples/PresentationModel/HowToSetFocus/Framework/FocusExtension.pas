unit FocusExtension;

interface

uses
  Classes,
  Controls,
  SysUtils,
  DSharp.Core.DependencyProperty,
  DSharp.Logging;

type
  ///	<summary>
  ///	  Helpers for setting focus to the UI control
  ///	</summary>
  TFocusExtension = class
  strict private
    class var FIsFocused: DependencyProperty<Boolean>;
    class var FLog: ILog;
    class function GetLog(): ILog; static;
  protected
    class property Log: ILog read GetLog;
  public
    class constructor Create;

    ///	<summary>
    ///	  A dependency property for setting focus to the UI control
    ///	</summary>
    class property IsFocused: DependencyProperty<Boolean> read FIsFocused;
  end;

implementation

uses
  DSharp.PresentationModel.Execute;

{ TFocusExtension }

class constructor TFocusExtension.Create;
begin
  FIsFocused := TDependencyProperty.RegisterAttached('IsFocused',
    TypeInfo(Boolean), TFocusExtension, TPropertyMetadata.Create(
    procedure(Sender: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs)
    var
      LControl: TWinControl;
      LAsBoolean: Boolean;
    begin
      LControl := Sender as TWinControl;
      LAsBoolean := EventArgs.NewValue.AsBoolean;
      Log.LogMessage
        ('Handling dependency property for %s:%s with property value %s',
        [LControl.Name, LControl.QualifiedClassName, BoolToStr(LAsBoolean)]);
      if LAsBoolean then
      begin
        // Execute action on background thread, ensuring the main UI thread is initialized by the time focus is set
        Execute.BeginOnUIThread(
          procedure
          begin
            LControl.SetFocus; // Don't care about false values.
            { TODO -o##jwp -cEnhance : Would like to use `OwnedNamePath` here: }
            Log.LogMessage('Set focus to %s:%s', [LControl.Name,
              LControl.QualifiedClassName]);

            // We need to switch it back to false right away so we can detect future change notifications back to true.
            // This way, the like a one-shot trigger.
            IsFocused.SetValue(Sender, False);
          end);
      end;
    end));
end;

class function TFocusExtension.GetLog(): ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(TFocusExtension));
  end;
  Result := FLog;
end;

initialization

TFocusExtension.ClassName;

end.
