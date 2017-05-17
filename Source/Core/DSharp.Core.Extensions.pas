(*
  Copyright (c) 2011-2012, DSharp team
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.Extensions;

interface

uses
  Classes,
  Spring, // keep order as we do not want TPropertyChangedEvent from this unit
  DSharp.Core.DependencyProperty,
  Rtti;

type
  TComponentHelper = class helper for TComponent
  private
    function GetDataContext: TObject;
    function GetIsComponentInitialized: Boolean;
    function GetOnDataContextChanged: IEvent<TPropertyChangedEvent>;
    procedure SetDataContext(const Value: TObject);
    procedure SetIsComponentInitialized(const Value: Boolean);
  public
    ///	<summary>
    ///	  Clears the local value of a property. The property to be cleared is
    ///	  specified by a DependencyProperty identifier.
    ///	</summary>
    procedure ClearValue(Prop: TDependencyProperty);

    ///	<summary>
    ///	  Returns the current effective value of a dependency property on this
    ///	  instance of a TComponent.
    ///	</summary>
    ///	<returns>
    ///	  Returns the current effective value.
    ///	</returns>
    function GetValue(Prop: TDependencyProperty): TValue;

    ///	<summary>
    ///	  Sets the local value of a dependency property, specified by its
    ///	  dependency property identifier.
    ///	</summary>
    procedure SetValue(Prop: TDependencyProperty; const Value: TValue);

    ///	<summary>
    ///	  Gets or sets the data context for a TComponent when it participates
    ///	  in data binding.
    ///	</summary>
    property DataContext: TObject read GetDataContext write SetDataContext;

    ///	<summary>
    ///	  Gets or sets if a TComponent is already initialized.
    ///	</summary>
    property IsComponentInitialized: Boolean read GetIsComponentInitialized
      write SetIsComponentInitialized;

    ///	<summary>
    ///	  Occurs when the data context for this element changes.
    ///	</summary>
    property OnDataContextChanged: IEvent<TPropertyChangedEvent>
      read GetOnDataContextChanged;
  end;

implementation

uses
  Spring.Events;

type
  TComponentExtensions = class
  private
    class var
      FDataContextProperty: TDependencyProperty;
      FIsComponentInitializedProperty: TDependencyProperty;
      FOnDataContextChangedProperty: TDependencyProperty;
    class procedure DoDataContextChanged(Component: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
  public
    class constructor Create;

    class property DataContextProperty: TDependencyProperty read FDataContextProperty;
    class property IsComponentInitializedProperty: TDependencyProperty
      read FIsComponentInitializedProperty;
    class property OnDataContextChangedProperty: TDependencyProperty
      read FOnDataContextChangedProperty;
  end;

{ TComponentHelper }

procedure TComponentHelper.ClearValue(Prop: TDependencyProperty);
begin
  Prop.ClearValue(Self);
end;

function TComponentHelper.GetDataContext: TObject;
begin
  Result := TComponentExtensions.DataContextProperty.GetValue(Self).AsObject;
end;

function TComponentHelper.GetIsComponentInitialized: Boolean;
begin
  Result := TComponentExtensions.IsComponentInitializedProperty.GetValue(Self).AsBoolean;
end;

function TComponentHelper.GetOnDataContextChanged: IEvent<TPropertyChangedEvent>;
var
  LValue: TValue;
  LEvent: IEvent<TPropertyChangedEvent>;
begin
  LValue := TComponentExtensions.OnDataContextChangedProperty.GetValue(Self);
  if LValue.IsEmpty then
  begin
    LEvent := TEvent<TPropertyChangedEvent>.Create;

    TComponentExtensions.OnDataContextChangedProperty.SetValue(Self,
      TValue.From<IEvent<TPropertyChangedEvent>>(LEvent));
    Result := LEvent;
  end
  else
  begin
    Result := LValue.AsType<IEvent<TPropertyChangedEvent>>;
  end;
end;

function TComponentHelper.GetValue(Prop: TDependencyProperty): TValue;
begin
  Result := Prop.GetValue(Self);
end;

procedure TComponentHelper.SetDataContext(const Value: TObject);
begin
  TComponentExtensions.DataContextProperty.SetValue(Self, Value);
end;

procedure TComponentHelper.SetIsComponentInitialized(const Value: Boolean);
begin
  TComponentExtensions.IsComponentInitializedProperty.SetValue(Self, Value);
end;

procedure TComponentHelper.SetValue(Prop: TDependencyProperty; const Value: TValue);
begin
  Prop.SetValue(Self, Value);
end;

{ TComponentExtensions }

class constructor TComponentExtensions.Create;
begin
  FDataContextProperty := TDependencyProperty.RegisterAttached('DataContext',
    TypeInfo(TObject), TComponent, TPropertyMetadata.Create(
    [TFrameworkPropertyMetadataOption.Inherits], DoDataContextChanged));

  FIsComponentInitializedProperty := TDependencyProperty.RegisterAttached(
    'IsComponentInitialized', TypeInfo(Boolean), TComponent, TPropertyMetadata.Create(False));

  FOnDataContextChangedProperty := TDependencyProperty.RegisterAttached('OnDataContextChanged',
    TypeInfo(IEvent<TPropertyChangedEvent>), TComponent,
    TPropertyMetadata.Create([TFrameworkPropertyMetadataOption.Inherits]));
end;

class procedure TComponentExtensions.DoDataContextChanged(Component: TComponent;
  EventArgs: IDependencyPropertyChangedEventArgs);
begin
  Component.OnDataContextChanged.Invoke(Component, EventArgs);
end;

initialization
  TComponentExtensions.ClassName;

end.
