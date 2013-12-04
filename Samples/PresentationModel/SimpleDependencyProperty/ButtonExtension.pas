unit ButtonExtension;

interface

uses
  Classes,
  SysUtils,
  DSharp.Core.DependencyProperty,
  DSharp.Core.Events,
  StdCtrls;

type
  ///	<summary>
  ///	  Helper for setting the button's description
  ///	</summary>
  ///	<remarks>
  ///	  Creating class helpers for dependency properties is not necessary. You
  ///	  can set dependency properties directly.
  ///	</remarks>
  TButtonHelper = class helper for TButton
  strict private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  public
    ///	<summary>
    ///	  Gets or sets the Description property.
    ///	</summary>
    property Description: string read GetDescription write SetDescription;
  end;

  ///	<summary>
  ///	  Host class for Description dependency property
  ///	</summary>
  TButtonExtension = class
  private
  class var
    class var FDescriptionProperty: DependencyProperty<string>;
    class procedure DescriptionChanged(Sender: TComponent;
      EventArgs: IDependencyPropertyChangedEventArgs);
  public
    class constructor Create;

    ///	<summary>
    ///	  Description Dependency Property
    ///	</summary>
    class property DescriptionProperty: DependencyProperty<string>
      read FDescriptionProperty;
  end;

implementation

function TButtonHelper.GetDescription: string;
begin
  Result := TButtonExtension.DescriptionProperty.GetValue(Self);
end;

procedure TButtonHelper.SetDescription(const Value: string);
begin
  TButtonExtension.DescriptionProperty.SetValue(Self, Value);
end;

{ TButtonExtension }

class constructor TButtonExtension.Create;
begin
  FDescriptionProperty := TDependencyProperty.RegisterAttached( //
    'Description', // Property name
    TypeInfo(string), // //Property type
    TButton, // Type of the dependency property provider
    TPropertyMetadata.Create(DescriptionChanged)
    // Default value and callback invoked on property value has changed
    );
end;

class procedure TButtonExtension.DescriptionChanged(Sender: TComponent;
  EventArgs: IDependencyPropertyChangedEventArgs);
var
  LAttachedObject: TButton;
begin
  try
    LAttachedObject := Sender as TButton;
    if Assigned(LAttachedObject) then
    begin
      // Do some processing here, for example
      LAttachedObject.ShowHint := True;
      LAttachedObject.Hint := EventArgs.NewValue.AsString;
    end;
  except
    on E: Exception do;
  end;
end;

initialization

TButtonExtension.ClassName;

end.
