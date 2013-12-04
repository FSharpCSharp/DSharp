unit DSharp.PresentationModel.ViewAttachedEventArgs;

interface

uses
  Rtti,
  DSharp.Core.EventArgs,
  DSharp.PresentationModel.ViewAttachedEventArgsIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IViewAttachedEventArgs" />
  ///	</summary>
  TViewAttachedEventArgs = class(TEventArgs, IViewAttachedEventArgs)
  private
    FView: TObject;
    FContext: TValue;
    function GetView: TObject;
    function GetContext: TValue;
  public
    constructor Create(View: TObject; Context: TValue);
  end;

implementation

{ TViewAttachedEventArgs }

constructor TViewAttachedEventArgs.Create(View: TObject; Context: TValue);
begin
  FView := View;
  FContext := Context;
end;

function TViewAttachedEventArgs.GetContext: TValue;
begin
  Result := FContext;
end;

function TViewAttachedEventArgs.GetView: TObject;
begin
  Result := FView;
end;

end.
