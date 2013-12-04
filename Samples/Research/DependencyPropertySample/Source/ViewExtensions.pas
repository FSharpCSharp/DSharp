unit ViewExtensions;

interface

uses
  DSharp.Core.DependencyProperty,
  DSharp.PresentationModel;

type
  TViewExtensions = class
  strict private
    class var FModelProperty: TDependencyProperty;
    class procedure OnModelChanged(targetLocation: TComponent; args: IDependencyPropertyChangedEventArgs);
  public
    class procedure Init;
    /// <summary>
    /// A dependency property for attaching a model to the UI.
    /// </summary>
    class property ModelProperty: TDependencyProperty read FModelProperty write FModelProperty;
  end;

implementation

uses
  SysUtils;

{ TViewExtensions }

class procedure TViewExtensions.Init;
begin
  FModelProperty := TDependencyProperty.RegisterAttached('Model',
    TypeInfo(TObject), TViewExtensions, TPropertyMetadata.Create(OnModelChanged));
end;

class procedure TViewExtensions.OnModelChanged(targetLocation: TComponent;
  args: IDependencyPropertyChangedEventArgs);
begin
  // raise Exception.Create('Mission accomplished!');
end;

initialization
  TViewExtensions.Init;

end.
