unit DSharp.PresentationModel.Action;

interface

uses
  Classes,
  DSharp.Core.DependencyProperty,
  DSharp.Logging;

type
  ///	<summary>
  ///	  A host for action related attached properties.
  ///	</summary>
  Action = class
  private
  class var
    FLog: ILog;
    FTargetProperty: TDependencyProperty;
    FTargetWithoutContextProperty: TDependencyProperty;

    class function GetLog: ILog; static;
    class procedure OnTargetChanged(DependencyObject: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
    class procedure OnTargetWithoutContextChanged(DependencyObject: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
    class procedure SetTargetCore(EventArgs
      : IDependencyPropertyChangedEventArgs; DependencyObject: TComponent;
      SetContext: Boolean);

    class property Log: ILog read GetLog;
  public
    class constructor Create;

    ///	<summary>
    ///	  Gets the target for instances of <see cref="TComponent" /> .
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The element to which the target is attached.
    ///	</param>
    ///	<returns>
    ///	  The target for instances of <see cref="TComponent" />
    ///	</returns>
    class function GetTarget(DependencyObject: TComponent): TObject;

    ///	<summary>
    ///	  Gets the target for instances of <see cref="TComponent" /> .
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The element to which the target is attached.
    ///	</param>
    ///	<returns>
    ///	  The target for instances of <see cref="TComponent" />
    ///	</returns>
    class function GetTargetWithoutContext(DependencyObject
      : TComponent): TObject;

    ///	<summary>
    ///	  Sets the target of the <see cref="TComponent" /> .
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The element to attach the target to.
    ///	</param>
    ///	<param name="Target">
    ///	  The target for instances of <see cref="TComponent" /> .
    ///	</param>
    class procedure SetTarget(DependencyObject: TComponent; Target: TObject);

    ///	<summary>
    ///	  Sets the target of the <see cref="TComponent" /> .
    ///	</summary>
    ///	<param name="d">
    ///	  The element to attach the target to.
    ///	</param>
    ///	<param name="target">
    ///	  The target for instances of <see cref="TComponent" /> .
    ///	</param>
    ///	<remarks>
    ///	  The DataContext will not be set.
    ///	</remarks>
    class procedure SetTargetWithoutContext(DependencyObject: TComponent;
      Target: TObject);

    ///	<summary>
    ///	  A property definition representing the target of an
    ///	  <see cref="TComponent" /> . The DataContext of the element will be
    ///	  set to this instance.
    ///	</summary>
    class property TargetProperty: TDependencyProperty read FTargetProperty;

    ///	<summary>
    ///	  A property definition representing the target of an
    ///	  <see cref="TComponent" /> . The DataContext of the element is not set
    ///	  to this instance.
    ///	</summary>
    class property TargetWithoutContextProperty: TDependencyProperty
      read FTargetWithoutContextProperty;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Extensions,
  DSharp.PresentationModel.IoC,
  SysUtils;

class constructor Action.Create;
begin
  FTargetProperty := TDependencyProperty.RegisterAttached('Target',
    TypeInfo(TObject), Action, TPropertyMetadata.Create(nil, OnTargetChanged));
  FTargetWithoutContextProperty := TDependencyProperty.RegisterAttached
    ('TargetWithoutContext', TypeInfo(TObject), Action,
    TPropertyMetadata.Create(nil, OnTargetWithoutContextChanged));
end;

class function Action.GetLog: ILog;
begin
  if not Assigned(FLog) then
  begin
    FLog := LogManager.GetLog(TypeInfo(Action));
  end;
  Result := FLog;
end;

class function Action.GetTarget(DependencyObject: TComponent): TObject;
begin
  Result := TargetProperty.GetValue(DependencyObject).AsObject;
end;

class function Action.GetTargetWithoutContext(DependencyObject
  : TComponent): TObject;
begin
  Result := TargetWithoutContextProperty.GetValue(DependencyObject).AsObject;
end;

class procedure Action.OnTargetChanged(DependencyObject: TComponent;
  EventArgs: IDependencyPropertyChangedEventArgs);
begin
  SetTargetCore(EventArgs, DependencyObject, True);
end;

class procedure Action.OnTargetWithoutContextChanged(DependencyObject
  : TComponent; EventArgs: IDependencyPropertyChangedEventArgs);
begin
  SetTargetCore(EventArgs, DependencyObject, False);
end;

class procedure Action.SetTarget(DependencyObject: TComponent; Target: TObject);
begin
  TargetProperty.SetValue(DependencyObject, Target);
end;

class procedure Action.SetTargetCore(EventArgs
  : IDependencyPropertyChangedEventArgs; DependencyObject: TComponent;
  SetContext: Boolean);
var
  LContainerKey: string;
  LTarget: TObject;
begin
  if EventArgs.NewValue.IsEmpty or SameValue(EventArgs.NewValue,
    EventArgs.OldValue) then
    Exit;

  LTarget := EventArgs.NewValue.AsObject;

  if (LContainerKey <> EmptyStr) and EventArgs.NewValue.TryAsType<string>
    (LContainerKey) then
  begin
    LTarget := IoC.GetInstance(nil, LContainerKey).AsObject;
  end;

  if SetContext then
  begin
    Log.LogMessage('Setting DataContext of %s to %s.',
      [DependencyObject.ToString, LTarget.ToString]);
    DependencyObject.DataContext := LTarget;

    // In FireMonkey TForm acts as a root container for actual view which is its first and only child
    // The actual view (first control on a form) is displayed by setting it's Parent property to another visual control
    // Because of this DP which is set on the TForm can never be located
    // Workaround:
    // Set DataContext directly on the first actual view (first control on a form). The following line is not relevant to VCL.
    // It would be better to use Controls[0] but this is platform agnostic code...
    if DependencyObject.ComponentCount > 0 then
    begin
      DependencyObject.Components[0].DataContext := LTarget;
    end;
  end;

  // Removed because we have no support for action messages
  // Log.Info(" Attaching message handler { 0 } to { 1 }.", target, d);
  // Message.SetHandler(d, target);
end;

class procedure Action.SetTargetWithoutContext(DependencyObject: TComponent;
  Target: TObject);
begin
  TargetWithoutContextProperty.SetValue(DependencyObject, Target);
end;

end.
