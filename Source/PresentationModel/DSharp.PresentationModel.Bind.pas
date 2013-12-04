unit DSharp.PresentationModel.Bind;

interface

uses
  Classes,
  DSharp.Core.DependencyProperty;

type
  ///	<summary>
  ///	  Hosts dependency properties for binding.
  ///	</summary>
  Bind = class
  private
  class var
    FModelProperty: TDependencyProperty;
    FModelWithoutContextProperty: TDependencyProperty;
    FNoContextProperty: TDependencyProperty;

    class procedure ModelChanged(DependencyObject: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
    class procedure ModelWithoutContextChanged(DependencyObject: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
  public
    class constructor Create;

    ///	<summary>
    ///	  Gets the model to bind to.
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The dependency object to bind to.
    ///	</param>
    ///	<returns>
    ///	  The model.
    ///	</returns>
    class function GetModelWithoutContext(DependencyObject: TComponent)
      : TObject;

    ///	<summary>
    ///	  Sets the model to bind to.
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The dependency object to bind to.
    ///	</param>
    ///	<param name="value">
    ///	  The model.
    ///	</param>
    class procedure SetModelWithoutContext(DependencyObject: TComponent;
      value: TObject);

    ///	<summary>
    ///	  Gets the model to bind to.
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The dependency object to bind to.
    ///	</param>
    ///	<returns>
    ///	  The model.
    ///	</returns>
    class function GetModel(DependencyObject: TComponent): TObject;

    ///	<summary>
    ///	  Sets the model to bind to.
    ///	</summary>
    ///	<param name="DependencyObject">
    ///	  The dependency object to bind to.
    ///	</param>
    ///	<param name="value">
    ///	  The model.
    ///	</param>
    class procedure SetModel(DependencyObject: TComponent; value: TObject);

    ///	<summary>
    ///	  Occurs when the data context for this element changes.
    ///	</summary>
    class procedure DataContextChanged(d: TComponent;
      e: IDependencyPropertyChangedEventArgs);

    ///	<summary>
    ///	  Allows binding on an existing view. Use this on root UserControls,
    ///	  Pages and Windows; not in a DataTemplate.
    ///	</summary>
    class property ModelProperty: TDependencyProperty read FModelProperty
      write FModelProperty;

    ///	<summary>
    ///	  Allows binding on an existing view without setting the data context.
    ///	  Use this from within a DataTemplate.
    ///	</summary>
    class property ModelWithoutContextProperty: TDependencyProperty
      read FModelWithoutContextProperty write FModelWithoutContextProperty;

    ///	<summary>
    ///	  Allows binding on an existing view. Use this on root UserControls,
    ///	  Pages and Windows; not in a DataTemplate.
    ///	</summary>
    class property NoContextProperty: TDependencyProperty
      read FNoContextProperty;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.IoC,
  DSharp.PresentationModel.RoutedEvent,
  DSharp.PresentationModel.View,
  DSharp.PresentationModel.ViewModelBinder,
  StrUtils,
  SysUtils,
  Rtti;

{ Bind }

class constructor Bind.Create;
begin
  FModelProperty := TDependencyProperty.RegisterAttached('Model',
    TypeInfo(TObject), Bind, TPropertyMetadata.Create(nil, ModelChanged));
  FModelWithoutContextProperty := TDependencyProperty.RegisterAttached
    ('ModelWithoutContext', TypeInfo(TObject), Bind,
    TPropertyMetadata.Create(nil, ModelWithoutContextChanged));
  FNoContextProperty := TDependencyProperty.RegisterAttached('NoContext',
    TypeInfo(Boolean), Bind, TPropertyMetadata.Create(False));
end;

class procedure Bind.DataContextChanged(d: TComponent;
  e: IDependencyPropertyChangedEventArgs);
begin

end;

class function Bind.GetModel(DependencyObject: TComponent): TObject;
begin
  Result := ModelProperty.GetValue(DependencyObject).AsObject;
end;

class function Bind.GetModelWithoutContext(DependencyObject
  : TComponent): TObject;
begin
  Result := ModelWithoutContextProperty.GetValue(DependencyObject).AsObject;
end;

class procedure Bind.ModelChanged(DependencyObject: TComponent;
  EventArgs: IDependencyPropertyChangedEventArgs);
var
  LElement: TComponent;
  LContext: TValue;
begin
  if EventArgs.NewValue.IsEmpty or SameValue(EventArgs.OldValue,
    EventArgs.NewValue) then
    Exit;

  LElement := DependencyObject;
  if not Assigned(LElement) then
    Exit;

  View.ExecuteOnLoad(LElement,
    procedure(ASender: TObject; AArgs: IRoutedEventArgs)
    var
      LTarget: TObject;
      LContainerKey: string;
    begin
      LTarget := EventArgs.NewValue.AsObject;

      if (LContainerKey <> EmptyStr) and EventArgs.NewValue.TryAsType<string>
        (LContainerKey) then
        LTarget := IoC.GetInstance(nil, LContainerKey).AsObject;

      View.IsScopeRootProperty.SetValue(DependencyObject, True);

      LContext := IfThen(LElement.Name = EmptyStr,
        IntToStr(LElement.GetHashCode), LElement.Name);

      ViewModelBinder.Bind(LTarget, DependencyObject, LContext);
    end);
end;

class procedure Bind.ModelWithoutContextChanged(DependencyObject: TComponent;
EventArgs: IDependencyPropertyChangedEventArgs);
var
  LElement: TComponent;
  LContext: TValue;
begin
  if EventArgs.NewValue.IsEmpty or SameValue(EventArgs.OldValue,
    EventArgs.NewValue) then
    Exit;

  LElement := DependencyObject;
  if not Assigned(LElement) then
    Exit;

  View.ExecuteOnLoad(LElement,
    procedure(ASender: TObject; AArgs: IRoutedEventArgs)
    var
      LTarget: TObject;
      LContainerKey: string;
    begin
      LTarget := EventArgs.NewValue.AsObject;

      if (LContainerKey <> EmptyStr) and EventArgs.NewValue.TryAsType<string>
        (LContainerKey) then
        LTarget := IoC.GetInstance(nil, LContainerKey).AsObject;

      View.IsScopeRootProperty.SetValue(DependencyObject, True);

      LContext := IfThen(LElement.Name = EmptyStr,
        IntToStr(LElement.GetHashCode), LElement.Name);

      NoContextProperty.SetValue(DependencyObject, True);
      ViewModelBinder.Bind(LTarget, DependencyObject, LContext);
    end);
end;

class procedure Bind.SetModel(DependencyObject: TComponent; value: TObject);
begin
  ModelProperty.SetValue(DependencyObject, value);
end;

class procedure Bind.SetModelWithoutContext(DependencyObject: TComponent;
value: TObject);
begin
  ModelWithoutContextProperty.SetValue(DependencyObject, value);
end;

initialization

Bind.ClassName;

end.
