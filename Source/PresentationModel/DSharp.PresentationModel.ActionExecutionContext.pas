unit DSharp.PresentationModel.ActionExecutionContext;

interface

uses
  Classes,
  Rtti,
  DSharp.PresentationModel.ActionExecutionContextIntf;

type
  ///	<summary>
  ///	  An implementation of <see cref="IActionExecutionContext" />
  ///	</summary>
  TActionExecutionContext = class(TInterfacedObject, IActionExecutionContext)
  private
    FMethod: TRttiMethod;
    FSender: TObject;
    FTarget: TObject;
    FView: TComponent;
    {$REGION 'Property Accessors'}
    function GetMethod: TRttiMethod;
    function GetSender: TObject;
    function GetTarget: TObject;
    function GetView: TComponent;
    procedure SetMethod(const Value: TRttiMethod);
    procedure SetSender(const Value: TObject);
    procedure SetView(const Value: TComponent);
    procedure SetTarget(const Value: TObject);
    {$ENDREGION}
  end;

implementation

{ TActionExecutionContext }

function TActionExecutionContext.GetMethod: TRttiMethod;
begin
  Result := FMethod;
end;

function TActionExecutionContext.GetSender: TObject;
begin
  Result := FSender;
end;

function TActionExecutionContext.GetTarget: TObject;
begin
  Result := FTarget;
end;

function TActionExecutionContext.GetView: TComponent;
begin
  Result := FView;
end;

procedure TActionExecutionContext.SetMethod(const Value: TRttiMethod);
begin
  FMethod := Value;
end;

procedure TActionExecutionContext.SetSender(const Value: TObject);
begin
  FSender := Value;
end;

procedure TActionExecutionContext.SetTarget(const Value: TObject);
begin
  FTarget := Value;
end;

procedure TActionExecutionContext.SetView(const Value: TComponent);
begin
  FView := Value;
end;

end.
